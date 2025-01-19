------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--           S Y S T E M . T R A C E B A C K . S Y M B O L I C              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 1999-2025, AdaCore                     --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  Run-time symbolic traceback support for targets using DWARF debug data

with Ada.Unchecked_Deallocation;

with Ada.Exceptions.Traceback; use Ada.Exceptions.Traceback;
with Ada.Containers.Generic_Array_Sort;

with System.Address_To_Access_Conversions;
with System.Soft_Links;
with System.CRTL;
with System.Dwarf_Lines;
with System.Exception_Traces;
with System.Standard_Library;
with System.Traceback_Entries;
with System.Strings;
with System.Bounded_Strings;
with Interfaces.C;

package body System.Traceback.Symbolic is

   use System.Bounded_Strings;
   use System.Dwarf_Lines;

   subtype Big_String is String (Positive);
   --  To deal with C strings

   package Big_String_Conv is new System.Address_To_Access_Conversions
     (Big_String);

   type Module_Cache;
   type Module_Cache_Acc is access all Module_Cache;

   type Module_Cache is record
      Name : Strings.String_Access;
      --  Name of the module

      C : Dwarf_Context (In_Exception => True);
      --  Context to symbolize an address within this module

      Chain : Module_Cache_Acc;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation
     (Module_Cache,
      Module_Cache_Acc);

   Cache_Chain : Module_Cache_Acc;
   --  Simply linked list of modules

   type Module_Array is array (Natural range <>) of Module_Cache_Acc;
   type Module_Array_Acc is access Module_Array;

   Modules_Cache : Module_Array_Acc;
   --  Sorted array of cached modules (if not null)

   Exec_Module : aliased Module_Cache;
   --  Context for the executable

   type Init_State is (Uninitialized, Initialized, Failed);
   Exec_Module_State : Init_State := Uninitialized;
   --  How Exec_Module is initialized

   procedure Init_Exec_Module;
   --  Initialize Exec_Module if not already initialized

   function Symbolic_Traceback
     (Traceback    : System.Traceback_Entries.Tracebacks_Array;
      Suppress_Hex : Boolean) return String;
   function Symbolic_Traceback
     (E            : Ada.Exceptions.Exception_Occurrence;
      Suppress_Hex : Boolean) return String;
   --  Suppress_Hex means do not print any hexadecimal addresses, even if the
   --  symbol is not available.

   function Lt (Left, Right : Module_Cache_Acc) return Boolean;
   --  Sort function for Module_Cache

   procedure Init_Module
     (Module       : out Module_Cache;
      Success      : out Boolean;
      Module_Name  :     String;
      Load_Address :     Address := Null_Address);
   --  Initialize Module

   procedure Close_Module (Module : in out Module_Cache);
   --  Finalize Module

   function Value (Item : System.Address) return String;
   --  Return the String contained in Item, up until the first NUL character

   pragma Warnings (Off, "*Add_Module_To_Cache*");
   procedure Add_Module_To_Cache (Module_Name : String;
                                  Load_Address : System.Address);
   --  To be called by Build_Cache_For_All_Modules to add a new module to the
   --  list. May not be referenced.

   package Module_Name is

      procedure Build_Cache_For_All_Modules;
      --  Create the cache for all current modules

      function Get (Addr : System.Address;
                    Load_Addr : access System.Address) return String;
      --  Returns the module name for the given address Addr, or an empty
      --  string for the main executable.  Load_Addr is set to the shared
      --  library load address if this information is available, or to
      --  System.Null_Address otherwise.

      function Is_Supported return Boolean;
      pragma Inline (Is_Supported);
      --  Returns True if Module_Name is supported, so if the traceback is
      --  supported for shared libraries.

   end Module_Name;

   package body Module_Name is separate;

   function Executable_Name return String;
   --  Returns the executable name as reported by argv[0]. If gnat_argv not
   --  initialized, return an empty string. If the argv[0] executable is not
   --  found in the PATH, return it unresolved.

   function Get_Executable_Load_Address return System.Address;
   pragma Import
     (C,
      Get_Executable_Load_Address,
      "__gnat_get_executable_load_address");
   --  Get the load address of the executable, or Null_Address if not known

   procedure Hexa_Traceback
     (Traceback    :        Tracebacks_Array;
      Suppress_Hex :        Boolean;
      Res          : in out Bounded_String);
   --  Non-symbolic traceback (simply write addresses in hexa)

   procedure Symbolic_Traceback_No_Lock
     (Traceback    :        Tracebacks_Array;
      Suppress_Hex :        Boolean;
      Res          : in out Bounded_String);
   --  Like the public Symbolic_Traceback_No_Lock except there is no provision
   --  against concurrent accesses.

   procedure Module_Symbolic_Traceback
     (Traceback    :        Tracebacks_Array;
      Module       :        Module_Cache;
      Suppress_Hex :        Boolean;
      Res          : in out Bounded_String);
   --  Returns the Traceback for a given module

   procedure Multi_Module_Symbolic_Traceback
     (Traceback    :        Tracebacks_Array;
      Suppress_Hex :        Boolean;
      Res          : in out Bounded_String);
   --  Build string containing symbolic traceback for the given call chain

   procedure Multi_Module_Symbolic_Traceback
     (Traceback    :        Tracebacks_Array;
      Module       :        Module_Cache;
      Suppress_Hex :        Boolean;
      Res          : in out Bounded_String);
   --  Likewise but using Module

   Max_String_Length : constant := 4096;
   --  Arbitrary limit on Bounded_Str length

   -----------
   -- Value --
   -----------

   function Value (Item : System.Address) return String is
   begin
      if Item /= Null_Address then
         for J in Big_String'Range loop
            if Big_String_Conv.To_Pointer (Item) (J) = ASCII.NUL then
               return Big_String_Conv.To_Pointer (Item) (1 .. J - 1);
            end if;
         end loop;
      end if;

      return "";
   end Value;

   -------------------------
   -- Add_Module_To_Cache --
   -------------------------

   procedure Add_Module_To_Cache (Module_Name : String;
                                  Load_Address : System.Address)
   is
      Module  : Module_Cache_Acc;
      Success : Boolean;
   begin
      Module := new Module_Cache;
      Init_Module (Module.all, Success, Module_Name, Load_Address);
      if not Success then
         Free (Module);
         return;
      end if;
      Module.Chain := Cache_Chain;
      Cache_Chain  := Module;
   end Add_Module_To_Cache;

   ----------------------
   -- Init_Exec_Module --
   ----------------------

   procedure Init_Exec_Module is
   begin
      if Exec_Module_State = Uninitialized then
         declare
            Exec_Path : constant String  := Executable_Name;
            Exec_Load : constant Address := Get_Executable_Load_Address;
            Success   : Boolean;
         begin
            Init_Module (Exec_Module, Success, Exec_Path, Exec_Load);

            if Success then
               Exec_Module_State := Initialized;
            else
               Exec_Module_State := Failed;
            end if;
         end;
      end if;
   end Init_Exec_Module;

   --------
   -- Lt --
   --------

   function Lt (Left, Right : Module_Cache_Acc) return Boolean is
   begin
      return Low_Address (Left.C) < Low_Address (Right.C);
   end Lt;

   -----------------------------
   -- Module_Cache_Array_Sort --
   -----------------------------

   procedure Module_Cache_Array_Sort is new Ada.Containers.Generic_Array_Sort
     (Natural,
      Module_Cache_Acc,
      Module_Array,
      Lt);

   ------------------
   -- Enable_Cache --
   ------------------

   procedure Enable_Cache (Include_Modules : Boolean := False) is
   begin
      --  Can be called at most once
      if Cache_Chain /= null then
         return;
      end if;

      --  Add all modules
      Init_Exec_Module;

      if Exec_Module_State = Failed then
         raise Program_Error with
           "cannot enable cache, executable state initialization failed.";
      end if;

      Cache_Chain := Exec_Module'Access;

      if Include_Modules then
         Module_Name.Build_Cache_For_All_Modules;
      end if;

      --  Build and fill the array of modules
      declare
         Count  : Natural;
         Module : Module_Cache_Acc;
      begin
         for Phase in 1 .. 2 loop
            Count  := 0;
            Module := Cache_Chain;
            while Module /= null loop
               Count := Count + 1;

               if Phase = 1 then
                  Enable_Cache (Module.C);
               else
                  Modules_Cache (Count) := Module;
               end if;
               Module := Module.Chain;
            end loop;

            if Phase = 1 then
               Modules_Cache := new Module_Array (1 .. Count);
            end if;
         end loop;
      end;

      --  Sort the array
      Module_Cache_Array_Sort (Modules_Cache.all);
   end Enable_Cache;

   ---------------------
   -- Executable_Name --
   ---------------------

   function Executable_Name return String is
      --  We have to import gnat_argv as an Address to match the type of
      --  gnat_argv in the binder generated file. Otherwise, we get spurious
      --  warnings about type mismatch when LTO is turned on.

      Gnat_Argv : System.Address;
      pragma Import (C, Gnat_Argv, "gnat_argv");

      type Argv_Array is array (0 .. 0) of System.Address;
      package Conv is new System.Address_To_Access_Conversions (Argv_Array);

      function locate_exec_on_path
        (A : System.Address;
         Current_Dir_On_Win : Interfaces.C.int) return System.Address;
      pragma Import (C, locate_exec_on_path, "__gnat_locate_exec_on_path");

   begin
      if Gnat_Argv = Null_Address then
         return "";
      end if;

      --  See if we can resolve argv[0] to a full path (to a file that we will
      --  be able to open). If the resolution fails, we were probably spawned
      --  by an imprecise exec call, typically passing a mere file name as
      --  argv[0] for a program in the current directory with '.' not on PATH.
      --  Best we can do is fallback to argv[0] unchanged in this case. If we
      --  fail opening that downstream, we'll just bail out.

      declare
         Argv0 : constant System.Address :=
           Conv.To_Pointer (Gnat_Argv) (0);

         Resolved_Argv0 : constant System.Address :=
           locate_exec_on_path (Argv0, 0);

         Exe_Argv : constant System.Address :=
           (if Resolved_Argv0 /= System.Null_Address
            then Resolved_Argv0
            else Argv0);

         Result : constant String := Value (Exe_Argv);

      begin
         --  The buffer returned by locate_exec_on_path was allocated using
         --  malloc and we should release this memory.

         if Resolved_Argv0 /= Null_Address then
            System.CRTL.free (Resolved_Argv0);
         end if;

         return Result;
      end;
   end Executable_Name;

   ------------------
   -- Close_Module --
   ------------------

   procedure Close_Module (Module : in out Module_Cache) is
   begin
      Close (Module.C);
      Strings.Free (Module.Name);
   end Close_Module;

   -----------------
   -- Init_Module --
   -----------------

   procedure Init_Module
     (Module       : out Module_Cache;
      Success      : out Boolean;
      Module_Name  :     String;
      Load_Address :     Address := Null_Address)
   is
   begin
      --  Early return if the module is not known

      if Module_Name = "" then
         Success := False;
         return;
      end if;

      Open (Module_Name, Module.C, Success);

      --  If a module can't be opened just return now, we just cannot give more
      --  information in this case.

      if not Success then
         return;
      end if;

      Set_Load_Address (Module.C, Load_Address);

      Module.Name := new String'(Module_Name);
   end Init_Module;

   -------------------------------
   -- Module_Symbolic_Traceback --
   -------------------------------

   procedure Module_Symbolic_Traceback
     (Traceback    :        Tracebacks_Array;
      Module       :        Module_Cache;
      Suppress_Hex :        Boolean;
      Res          : in out Bounded_String)
   is
      Success : Boolean;
   begin
      if Symbolic.Module_Name.Is_Supported then
         Append (Res, '[');
         Append (Res, Module.Name.all);
         Append (Res, ']' & ASCII.LF);
      end if;

      Dwarf_Lines.Symbolic_Traceback
        (Module.C,
         Traceback,
         Suppress_Hex,
         Success,
         Res);

      if not Success then
         Hexa_Traceback (Traceback, Suppress_Hex, Res);
      end if;

      --  We must not allow an unhandled exception here, since this function
      --  may be installed as a decorator for all automatic exceptions.

   exception
      when others =>
         return;
   end Module_Symbolic_Traceback;

   -------------------------------------
   -- Multi_Module_Symbolic_Traceback --
   -------------------------------------

   procedure Multi_Module_Symbolic_Traceback
     (Traceback    :        Tracebacks_Array;
      Suppress_Hex :        Boolean;
      Res          : in out Bounded_String)
   is
      F : constant Natural := Traceback'First;
   begin
      if Traceback'Length = 0 or else Is_Full (Res) then
         return;
      end if;

      if Modules_Cache /= null then
         --  Search in the cache

         declare
            Addr        : constant Address := Traceback (F);
            Hi, Lo, Mid : Natural;
         begin
            Lo := Modules_Cache'First;
            Hi := Modules_Cache'Last;
            while Lo <= Hi loop
               Mid := (Lo + Hi) / 2;
               if Addr < Low_Address (Modules_Cache (Mid).C) then
                  Hi := Mid - 1;
               elsif Is_Inside (Modules_Cache (Mid).C, Addr) then
                  Multi_Module_Symbolic_Traceback
                    (Traceback,
                     Modules_Cache (Mid).all,
                     Suppress_Hex,
                     Res);
                  return;
               else
                  Lo := Mid + 1;
               end if;
            end loop;

            --  Not found
            Hexa_Traceback (Traceback (F .. F), Suppress_Hex, Res);
            Multi_Module_Symbolic_Traceback
              (Traceback (F + 1 .. Traceback'Last),
               Suppress_Hex,
               Res);
         end;
      else

         --  First try the executable
         if Is_Inside (Exec_Module.C, Traceback (F)) then
            Multi_Module_Symbolic_Traceback
              (Traceback,
               Exec_Module,
               Suppress_Hex,
               Res);
            return;
         end if;

         --  Otherwise, try a shared library
         declare
            Load_Addr : aliased System.Address;
            M_Name  : constant String :=
              Module_Name.Get (Addr => Traceback (F),
                               Load_Addr => Load_Addr'Access);
            Module  : Module_Cache;
            Success : Boolean;
         begin
            Init_Module (Module, Success, M_Name, Load_Addr);
            if Success then
               Multi_Module_Symbolic_Traceback
                 (Traceback,
                  Module,
                  Suppress_Hex,
                  Res);
               Close_Module (Module);
            else
               --  Module not found
               Hexa_Traceback (Traceback (F .. F), Suppress_Hex, Res);
               Multi_Module_Symbolic_Traceback
                 (Traceback (F + 1 .. Traceback'Last),
                  Suppress_Hex,
                  Res);
            end if;
         end;
      end if;
   end Multi_Module_Symbolic_Traceback;

   procedure Multi_Module_Symbolic_Traceback
     (Traceback    :        Tracebacks_Array;
      Module       :        Module_Cache;
      Suppress_Hex :        Boolean;
      Res          : in out Bounded_String)
   is
      Pos : Positive;
   begin
      --  Will symbolize the first address...

      Pos := Traceback'First + 1;

      --  ... and all addresses in the same module

      Same_Module :
      loop
         exit Same_Module when Pos > Traceback'Last;

         --  Get address to check for corresponding module name

         exit Same_Module when not Is_Inside (Module.C, Traceback (Pos));

         Pos := Pos + 1;
      end loop Same_Module;

      Module_Symbolic_Traceback
        (Traceback (Traceback'First .. Pos - 1),
         Module,
         Suppress_Hex,
         Res);
      Multi_Module_Symbolic_Traceback
        (Traceback (Pos .. Traceback'Last),
         Suppress_Hex,
         Res);
   end Multi_Module_Symbolic_Traceback;

   --------------------
   -- Hexa_Traceback --
   --------------------

   procedure Hexa_Traceback
     (Traceback    :        Tracebacks_Array;
      Suppress_Hex :        Boolean;
      Res          : in out Bounded_String)
   is
      use System.Traceback_Entries;
   begin
      if Suppress_Hex then
         Append (Res, "...");
         Append (Res, ASCII.LF);
      else
         for J in Traceback'Range loop
            Append_Address (Res, PC_For (Traceback (J)));
            Append (Res, ASCII.LF);
         end loop;
      end if;
   end Hexa_Traceback;

   --------------------------------
   -- Symbolic_Traceback_No_Lock --
   --------------------------------

   procedure Symbolic_Traceback_No_Lock
     (Traceback    :        Tracebacks_Array;
      Suppress_Hex :        Boolean;
      Res          : in out Bounded_String)
   is
   begin
      if Symbolic.Module_Name.Is_Supported then
         Multi_Module_Symbolic_Traceback (Traceback, Suppress_Hex, Res);
      else
         if Exec_Module_State = Failed then
            Append (Res, "Call stack traceback locations:" & ASCII.LF);
            Hexa_Traceback (Traceback, Suppress_Hex, Res);
         else
            Module_Symbolic_Traceback
              (Traceback,
               Exec_Module,
               Suppress_Hex,
               Res);
         end if;
      end if;
   end Symbolic_Traceback_No_Lock;

   ------------------------
   -- Symbolic_Traceback --
   ------------------------

   LDAD_Header : constant String := "Load address: ";
   --  Copied from Ada.Exceptions.Exception_Data

   function Symbolic_Traceback
     (Traceback    : Tracebacks_Array;
      Suppress_Hex : Boolean) return String
   is
      Load_Address : constant Address := Get_Executable_Load_Address;
      Res          : Bounded_String (Max_Length => Max_String_Length);

   begin
      System.Soft_Links.Lock_Task.all;
      Init_Exec_Module;
      if Load_Address /= Null_Address then
         Append (Res, LDAD_Header);
         Append_Address (Res, Load_Address);
         Append (Res, ASCII.LF);
      end if;
      Symbolic_Traceback_No_Lock (Traceback, Suppress_Hex, Res);
      System.Soft_Links.Unlock_Task.all;

      return To_String (Res);

   exception
      when others =>
         System.Soft_Links.Unlock_Task.all;
         raise;
   end Symbolic_Traceback;

   function Symbolic_Traceback
     (Traceback : System.Traceback_Entries.Tracebacks_Array) return String is
   begin
      return Symbolic_Traceback (Traceback, Suppress_Hex => False);
   end Symbolic_Traceback;

   function Symbolic_Traceback_No_Hex
     (Traceback : System.Traceback_Entries.Tracebacks_Array) return String is
   begin
      return Symbolic_Traceback (Traceback, Suppress_Hex => True);
   end Symbolic_Traceback_No_Hex;

   function Symbolic_Traceback
     (E            : Ada.Exceptions.Exception_Occurrence;
      Suppress_Hex : Boolean) return String
   is
   begin
      return Symbolic_Traceback
          (Ada.Exceptions.Traceback.Tracebacks (E),
           Suppress_Hex);
   end Symbolic_Traceback;

   function Symbolic_Traceback
     (E : Ada.Exceptions.Exception_Occurrence) return String
   is
   begin
      return Symbolic_Traceback (E, Suppress_Hex => False);
   end Symbolic_Traceback;

   function Symbolic_Traceback_No_Hex
     (E : Ada.Exceptions.Exception_Occurrence) return String is
   begin
      return Symbolic_Traceback (E, Suppress_Hex => True);
   end Symbolic_Traceback_No_Hex;

   Exception_Tracebacks_Symbolic : constant Integer;
   pragma Import
     (C,
      Exception_Tracebacks_Symbolic,
      "__gl_exception_tracebacks_symbolic");
   --  Boolean indicating whether symbolic tracebacks should be generated.

   use Standard_Library;
begin
   --  If this version of this package is available, and the binder switch -Es
   --  was given, then we want to use this as the decorator by default, and we
   --  want to turn on tracing for Unhandled_Raise_In_Main. Note that the user
   --  cannot have already set Exception_Trace, because the runtime library is
   --  elaborated before user-defined code.

   if Exception_Tracebacks_Symbolic /= 0 then
      Exception_Traces.Set_Trace_Decorator (Symbolic_Traceback'Access);
      pragma Assert (Exception_Trace = RM_Convention);
      Exception_Trace := Unhandled_Raise_In_Main;
   end if;
end System.Traceback.Symbolic;
