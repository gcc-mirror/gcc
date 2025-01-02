------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--              G N A T . E X C E P T I O N _ A C T I O N S                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2002-2025, Free Software Foundation, Inc.         --
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

with Ada.Unchecked_Conversion;
with System;
with System.Soft_Links;       use System.Soft_Links;
with System.Standard_Library; use System.Standard_Library;
with System.Exception_Table;  use System.Exception_Table;
with Interfaces.C;            use Interfaces.C;

package body GNAT.Exception_Actions is

   Global_Action : Exception_Action;
   pragma Import
     (Ada, Global_Action, "__gnat_exception_actions_global_action");
   pragma Atomic (Global_Action);
   --  Imported from Ada.Exceptions. Any change in the external name needs to
   --  be coordinated with a-exextr.adb.

   Global_Unhandled_Action : Exception_Action;
   pragma Import
     (Ada, Global_Unhandled_Action,
      "__gnat_exception_actions_global_unhandled_action");
   pragma Atomic (Global_Unhandled_Action);
   --  Imported from Ada.Exceptions. Any change in the external name needs to
   --  be coordinated with a-exextr.adb.

   Raise_Hook_Initialized : Boolean;
   pragma Import
     (Ada, Raise_Hook_Initialized, "__gnat_exception_actions_initialized");

   function To_Raise_Action is new Ada.Unchecked_Conversion
     (Exception_Action, Raise_Action);

   --  ??? Would be nice to have this in System.Standard_Library
   function To_Data is new Ada.Unchecked_Conversion
     (Exception_Id, Exception_Data_Ptr);
   function To_Id is new Ada.Unchecked_Conversion
     (Exception_Data_Ptr, Exception_Id);

   ----------------------------
   -- Register_Global_Action --
   ----------------------------

   procedure Register_Global_Action (Action : Exception_Action) is
   begin
      Global_Action := Action;
   end Register_Global_Action;

   --------------------------------------
   -- Register_Global_Unhandled_Action --
   --------------------------------------

   procedure Register_Global_Unhandled_Action (Action : Exception_Action) is
   begin
      Global_Unhandled_Action := Action;
   end Register_Global_Unhandled_Action;

   ------------------------
   -- Register_Id_Action --
   ------------------------

   procedure Register_Id_Action
     (Id     : Exception_Id;
      Action : Exception_Action)
   is
   begin
      if Id = Null_Id then
         raise Program_Error;
      end if;

      Lock_Task.all;
      To_Data (Id).Raise_Hook := To_Raise_Action (Action);
      Raise_Hook_Initialized := True;
      Unlock_Task.all;
   end Register_Id_Action;

   ---------------
   -- Core_Dump --
   ---------------

   procedure Core_Dump (Occurrence : Exception_Occurrence) is separate;

   ------------------------
   -- Exception_Language --
   ------------------------

   function Exception_Language
     (E : Exception_Occurrence)
     return Exception_Languages is

      Foreign_Exception : aliased Exception_Data;
      pragma Import
        (Ada, Foreign_Exception, "system__exceptions__foreign_exception");

      function To_Exception_Data_Ptr is new
        Ada.Unchecked_Conversion (Exception_Id, Exception_Data_Ptr);

      IdD : constant Exception_Data_Ptr
        := To_Exception_Data_Ptr (Exception_Identity (E));
      Lang : constant Character := IdD.Lang;

   begin
      if Lang in 'B' .. 'C' then
         return EL_Cpp;
      end if;

      if Lang /= 'A' then
         return EL_Unknown;
      end if;

      if IdD /= Foreign_Exception'Unchecked_Access then
         return EL_Ada;
      end if;

      declare
         function Get_Exception_Machine_Occurrence
           (E : Exception_Occurrence) return System.Address;
         pragma Import (Ada, Get_Exception_Machine_Occurrence,
                        "__gnat_get_exception_machine_occurrence");

         function Exception_Language_Is_CPlusPlus
           (E : System.Address)
           return C_bool;
         pragma Import (C, Exception_Language_Is_CPlusPlus,
                        "__gnat_exception_language_is_cplusplus");

         function Exception_Language_Is_Ada
           (E : System.Address)
           return C_bool;
         pragma Import (C, Exception_Language_Is_Ada,
                     "__gnat_exception_language_is_ada");

         Occurrence : constant System.Address
           := Get_Exception_Machine_Occurrence (E);
      begin
         if Exception_Language_Is_CPlusPlus (Occurrence) then
            return EL_Cpp;
         elsif Exception_Language_Is_Ada (Occurrence) then
            --  This might some day indicate third-party Ada exceptions.
            --  With GNAT exceptions, we should not reach this point.
            return EL_Ada;
         end if;
      end;

      return EL_Unknown;
   end Exception_Language;

   --------------------------
   -- Is_Foreign_Exception --
   --------------------------

   function Is_Foreign_Exception (E : Exception_Occurrence) return Boolean is
   begin
      return Exception_Language (E) /= EL_Ada;
   end Is_Foreign_Exception;

   ----------------
   -- Name_To_Id --
   ----------------

   function Name_To_Id (Name : String) return Exception_Id is
   begin
      return To_Id (Internal_Exception (Name, Create_If_Not_Exist => False));
   end Name_To_Id;

   ---------------------------------
   -- Registered_Exceptions_Count --
   ---------------------------------

   function Registered_Exceptions_Count return Natural renames
     System.Exception_Table.Registered_Exceptions_Count;

   -------------------------------
   -- Get_Registered_Exceptions --
   -------------------------------
   --  This subprogram isn't an iterator to avoid concurrency problems,
   --  since the exceptions are registered dynamically. Since we have to lock
   --  the runtime while computing this array, this means that any callback in
   --  an active iterator would be unable to access the runtime.

   procedure Get_Registered_Exceptions
     (List : out Exception_Id_Array;
      Last : out Integer)
   is
      Ids : Exception_Data_Array (List'Range);
   begin
      Get_Registered_Exceptions (Ids, Last);

      for L in List'First .. Last loop
         List (L) := To_Id (Ids (L));
      end loop;
   end Get_Registered_Exceptions;

end GNAT.Exception_Actions;
