------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       I N T E R F A C E S . C P P                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2005, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Tags;                use Ada.Tags;
with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;

package body Interfaces.CPP is

--  Structure of the Dispatch Table

--           +-----------------------+
--           |     Offset_To_Top     |
--           +-----------------------+
--           | Typeinfo_Ptr/TSD_Ptr  |----> Type Specific Data
--  Tag ---> +-----------------------+      +-------------------+
--           |        table of       |      | inheritance depth |
--           :     primitive ops     :      +-------------------+
--           |        pointers       |      |   expanded name   |
--           +-----------------------+      +-------------------+
--                                          |   external tag    |
--                                          +-------------------+
--                                          |   Hash table link |
--                                          +-------------------+
--                                          | Remotely Callable |
--                                          +-------------------+
--                                          | Rec Ctrler offset |
--                                          +-------------------+
--                                          | table of          |
--                                          :   ancestor        :
--                                          |      tags         |
--                                          +-------------------+

   --  The declarations below need (extensive) comments ???

   subtype Cstring is String (Positive);
   type Cstring_Ptr is access all Cstring;
   type Tag_Table is array (Natural range <>) of Vtable_Ptr;
   pragma Suppress_Initialization (Tag_Table);

   type Type_Specific_Data is record
      Idepth        : Natural;
      Expanded_Name : Cstring_Ptr;
      External_Tag  : Cstring_Ptr;
      HT_Link       : Tag;
      Ancestor_Tags : Tag_Table (Natural);
   end record;

   type Vtable_Entry is record
     Pfn : System.Address;
   end record;

   type Vtable_Entry_Array is array (Positive range <>) of Vtable_Entry;

   type VTable is record
      --  Offset_To_Top : Integer;
      --  Typeinfo_Ptr  : System.Address; -- TSD is currently also here???
      Prims_Ptr  : Vtable_Entry_Array (Positive);
   end record;
   --  Note: See comment in a-tags.adb explaining why the components
   --        Offset_To_Top and Typeinfo_Ptr have been commented out.
   --  -----------------------------------------------------------------------
   --  The size of the Prims_Ptr array actually depends on the tagged type to
   --  which it applies. For each tagged type, the expander computes the
   --  actual array size, allocates the Dispatch_Table record accordingly, and
   --  generates code that displaces the base of the record after the
   --  Typeinfo_Ptr component. For this reason the first two components have
   --  been commented in the previous declaration. The access to these
   --  components is done by means of local functions.

   ---------------------------
   -- Unchecked Conversions --
   ---------------------------

   type Int_Ptr is access Integer;

   function To_Int_Ptr is
      new Unchecked_Conversion (System.Address, Int_Ptr);

   function To_Cstring_Ptr is
     new Unchecked_Conversion (Address, Cstring_Ptr);

   function To_Address is
     new Unchecked_Conversion (Cstring_Ptr, Address);

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Length (Str : Cstring_Ptr) return Natural;
   --  Length of string represented by the given pointer (treating the string
   --  as a C-style string, which is Nul terminated).

   function Offset_To_Top (T : Vtable_Ptr) return Integer;
   --  Returns the current value of the offset_to_top component available in
   --  the prologue of the dispatch table.

   function Typeinfo_Ptr (T : Vtable_Ptr) return System.Address;
   --  Returns the current value of the typeinfo_ptr component available in
   --  the prologue of the dispatch table.

   pragma Unreferenced (Offset_To_Top);
   pragma Unreferenced (Typeinfo_Ptr);
   --  These functions will be used for full compatibility with the C++ ABI

   -----------------------
   -- CPP_CW_Membership --
   -----------------------

   function CPP_CW_Membership
     (Obj_Tag : Vtable_Ptr;
      Typ_Tag : Vtable_Ptr) return Boolean
   is
      Pos : constant Integer := TSD (Obj_Tag).Idepth - TSD (Typ_Tag).Idepth;
   begin
      return Pos >= 0 and then TSD (Obj_Tag).Ancestor_Tags (Pos) = Typ_Tag;
   end CPP_CW_Membership;

   ---------------------------
   -- CPP_Get_Expanded_Name --
   ---------------------------

   function CPP_Get_Expanded_Name (T : Vtable_Ptr) return Address is
   begin
      return To_Address (TSD (T).Expanded_Name);
   end CPP_Get_Expanded_Name;

   --------------------------
   -- CPP_Get_External_Tag --
   --------------------------

   function CPP_Get_External_Tag (T : Vtable_Ptr) return Address is
   begin
      return To_Address (TSD (T).External_Tag);
   end CPP_Get_External_Tag;

   -------------------------------
   -- CPP_Get_Inheritance_Depth --
   -------------------------------

   function CPP_Get_Inheritance_Depth (T : Vtable_Ptr) return Natural is
   begin
      return TSD (T).Idepth;
   end CPP_Get_Inheritance_Depth;

   -------------------------
   -- CPP_Get_Prim_Op_Address --
   -------------------------

   function CPP_Get_Prim_Op_Address
     (T        : Vtable_Ptr;
      Position : Positive) return Address
   is
   begin
      return T.Prims_Ptr (Position).Pfn;
   end CPP_Get_Prim_Op_Address;

   -----------------------
   -- CPP_Get_RC_Offset --
   -----------------------

   function CPP_Get_RC_Offset (T : Vtable_Ptr) return SSE.Storage_Offset is
      pragma Warnings (Off, T);
   begin
      return 0;
   end CPP_Get_RC_Offset;

   -------------------------------
   -- CPP_Get_Remotely_Callable --
   -------------------------------

   function CPP_Get_Remotely_Callable (T : Vtable_Ptr) return Boolean is
      pragma Warnings (Off, T);
   begin
      return True;
   end CPP_Get_Remotely_Callable;

   -----------------
   -- CPP_Get_TSD --
   -----------------

   function CPP_Get_TSD  (T : Vtable_Ptr) return Address is
      use type System.Storage_Elements.Storage_Offset;
      TSD_Ptr : constant Addr_Ptr :=
                  To_Addr_Ptr (To_Address (T) - CPP_DT_Typeinfo_Ptr_Size);
   begin
      return TSD_Ptr.all;
   end CPP_Get_TSD;

   --------------------
   -- CPP_Inherit_DT --
   --------------------

   procedure CPP_Inherit_DT
    (Old_T   : Vtable_Ptr;
     New_T   : Vtable_Ptr;
     Entry_Count : Natural)
   is
   begin
      if Old_T /= null then
         New_T.Prims_Ptr (1 .. Entry_Count)
           := Old_T.Prims_Ptr (1 .. Entry_Count);
      end if;
   end CPP_Inherit_DT;

   ---------------------
   -- CPP_Inherit_TSD --
   ---------------------

   procedure CPP_Inherit_TSD
     (Old_TSD : Address;
      New_Tag : Vtable_Ptr)
   is
      Old_TSD_Ptr : constant Type_Specific_Data_Ptr :=
                      To_Type_Specific_Data_Ptr (Old_TSD);

      New_TSD_Ptr : constant Type_Specific_Data_Ptr :=
                      TSD (New_Tag);

   begin
      if Old_TSD_Ptr /= null then
         New_TSD_Ptr.Idepth := Old_TSD_Ptr.Idepth + 1;
         New_TSD_Ptr.Ancestor_Tags (1 .. New_TSD_Ptr.Idepth) :=
           Old_TSD_Ptr.Ancestor_Tags (0 .. Old_TSD_Ptr.Idepth);
      else
         New_TSD_Ptr.Idepth := 0;
      end if;

      New_TSD_Ptr.Ancestor_Tags (0) := New_Tag;
   end CPP_Inherit_TSD;

   ---------------------------
   -- CPP_Set_Expanded_Name --
   ---------------------------

   procedure CPP_Set_Expanded_Name (T : Vtable_Ptr; Value : Address) is
   begin
      TSD (T).Expanded_Name := To_Cstring_Ptr (Value);
   end CPP_Set_Expanded_Name;

   --------------------------
   -- CPP_Set_External_Tag --
   --------------------------

   procedure CPP_Set_External_Tag (T : Vtable_Ptr; Value : Address) is
   begin
      TSD (T).External_Tag := To_Cstring_Ptr (Value);
   end CPP_Set_External_Tag;

   -------------------------------
   -- CPP_Set_Inheritance_Depth --
   -------------------------------

   procedure CPP_Set_Inheritance_Depth
     (T     : Vtable_Ptr;
      Value : Natural)
   is
   begin
      TSD (T).Idepth := Value;
   end CPP_Set_Inheritance_Depth;

   -----------------------------
   -- CPP_Set_Prim_Op_Address --
   -----------------------------

   procedure CPP_Set_Prim_Op_Address
     (T        : Vtable_Ptr;
      Position : Positive;
      Value    : Address)
   is
   begin
      T.Prims_Ptr (Position).Pfn := Value;
   end CPP_Set_Prim_Op_Address;

   -----------------------
   -- CPP_Set_RC_Offset --
   -----------------------

   procedure CPP_Set_RC_Offset (T : Vtable_Ptr; Value : SSE.Storage_Offset) is
      pragma Warnings (Off, T);
      pragma Warnings (Off, Value);
   begin
      null;
   end CPP_Set_RC_Offset;

   -------------------------------
   -- CPP_Set_Remotely_Callable --
   -------------------------------

   procedure CPP_Set_Remotely_Callable (T : Vtable_Ptr; Value : Boolean) is
      pragma Warnings (Off, T);
      pragma Warnings (Off, Value);
   begin
      null;
   end CPP_Set_Remotely_Callable;

   -----------------
   -- CPP_Set_TSD --
   -----------------

   procedure CPP_Set_TSD (T : Vtable_Ptr; Value : Address) is
      use type System.Storage_Elements.Storage_Offset;
      TSD_Ptr : constant Addr_Ptr :=
                  To_Addr_Ptr (To_Address (T) - CPP_DT_Typeinfo_Ptr_Size);
   begin
      TSD_Ptr.all := Value;
   end CPP_Set_TSD;

   --------------------
   -- Displaced_This --
   --------------------

   function Displaced_This
    (Current_This : System.Address;
     Vptr         : Vtable_Ptr;
     Position     : Positive)
     return         System.Address
   is
      pragma Warnings (Off, Vptr);
      pragma Warnings (Off, Position);

   begin
      return Current_This;

      --  why is the following here commented out ???
      --  + Storage_Offset (Vptr.Prims_Ptr (Position).Delta1);
   end Displaced_This;

   -------------------
   -- Expanded_Name --
   -------------------

   function Expanded_Name (T : Vtable_Ptr) return String is
      Result : constant Cstring_Ptr := TSD (T).Expanded_Name;
   begin
      return Result (1 .. Length (Result));
   end Expanded_Name;

   ------------------
   -- External_Tag --
   ------------------

   function External_Tag (T : Vtable_Ptr) return String is
      Result : constant Cstring_Ptr := TSD (T).External_Tag;
   begin
      return Result (1 .. Length (Result));
   end External_Tag;

   ------------
   -- Length --
   ------------

   function Length (Str : Cstring_Ptr) return Natural is
      Len : Integer := 1;

   begin
      while Str (Len) /= ASCII.Nul loop
         Len := Len + 1;
      end loop;

      return Len - 1;
   end Length;

   ------------------
   -- Offset_To_Top --
   ------------------

   function Offset_To_Top (T : Vtable_Ptr) return Integer is
      use type System.Storage_Elements.Storage_Offset;

      TSD_Ptr : constant Int_Ptr
        := To_Int_Ptr (To_Address (T) - CPP_DT_Prologue_Size);
   begin
      return TSD_Ptr.all;
   end Offset_To_Top;

   ------------------
   -- Typeinfo_Ptr --
   ------------------

   function Typeinfo_Ptr (T : Vtable_Ptr) return System.Address is
      use type System.Storage_Elements.Storage_Offset;
      TSD_Ptr : constant Addr_Ptr :=
                  To_Addr_Ptr (To_Address (T) - CPP_DT_Typeinfo_Ptr_Size);
   begin
      return TSD_Ptr.all;
   end Typeinfo_Ptr;

   ---------
   -- TSD --
   ---------

   function TSD (T : Vtable_Ptr) return Type_Specific_Data_Ptr is
   begin
      return To_Type_Specific_Data_Ptr (CPP_Get_TSD (T));
   end TSD;

end Interfaces.CPP;
