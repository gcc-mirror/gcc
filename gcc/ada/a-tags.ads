------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                             A D A . T A G S                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 1992-2001 Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

with System;
with System.Storage_Elements;

package Ada.Tags is

   pragma Elaborate_Body;

   type Tag is private;

   function Expanded_Name (T : Tag) return String;

   function External_Tag (T : Tag) return String;

   function Internal_Tag (External : String) return Tag;

   Tag_Error : exception;

private

   ----------------------------------------------------------------
   --  Abstract procedural interface for the GNAT dispatch table --
   ----------------------------------------------------------------

   --  GNAT's Dispatch Table format is customizable in order to match the
   --  format used in another language. GNAT supports programs that use
   --  two different dispatch table format at the same time: the native
   --  format that supports Ada 95 tagged types and which is described in
   --  Ada.Tags and a foreign format for types that are imported from some
   --  other language (typically C++) which is described in interfaces.cpp.
   --  The runtime information kept for each tagged type is separated into
   --  two objects: the Dispatch Table and the Type Specific Data record.
   --  These two objects are allocated statically using the constants:

   --      DT Size  = DT_Prologue_Size  + Nb_Prim * DT_Entry_Size
   --      TSD Size = TSD_Prologue_Size + (1 + Idepth)  * TSD_Entry_Size

   --  where Nb_prim is the number of primitive operations of the given
   --  type and Idepth its inheritance depth.

   --  The compiler generates calls to the following SET routines to
   --  initialize those structures and uses the GET functions to
   --  retreive the information when needed

   package S   renames System;
   package SSE renames System.Storage_Elements;

   function CW_Membership (Obj_Tag : Tag; Typ_Tag : Tag) return Boolean;
   --  Given the tag of an object and the tag associated to a type, return
   --  true if Obj is in Typ'Class.

   function Get_Expanded_Name (T : Tag) return S.Address;
   --  Retrieve the address of a null terminated string containing
   --  the expanded name

   function Get_External_Tag (T : Tag) return S.Address;
   --  Retrieve the address of a null terminated string containing
   --  the external name

   function Get_Prim_Op_Address
     (T        : Tag;
      Position : Positive)
      return     S.Address;
   --  Given a pointer to a dispatch Table (T) and a position in the DT
   --  this function returns the address of the virtual function stored
   --  in it (used for dispatching calls)

   function Get_Inheritance_Depth (T : Tag) return Natural;
   --  Given a pointer to a dispatch Table, retrieves the value representing
   --  the depth in the inheritance tree (used for membership).

   function Get_RC_Offset (T : Tag) return SSE.Storage_Offset;
   --  Return the Offset of the implicit record controller when the object
   --  has controlled components. O otherwise.

   pragma Export (Ada, Get_RC_Offset, "ada__tags__get_rc_offset");
   --  This procedure is used in s-finimp to compute the deep routines
   --  it is exported manually in order to avoid changing completely the
   --  organization of the run time.

   function Get_Remotely_Callable (T : Tag) return Boolean;
   --  Return the value previously set by Set_Remotely_Callable

   function  Get_TSD (T : Tag) return S.Address;
   --  Given a pointer T to a dispatch Table, retreives the address of the
   --  record containing the Type Specific Data generated by GNAT

   procedure Inherit_DT
    (Old_T   : Tag;
     New_T   : Tag;
     Entry_Count : Natural);
   --  Entry point used to initialize the DT of a type knowing the tag
   --  of the direct ancestor and the number of primitive ops that are
   --  inherited (Entry_Count).

   procedure Inherit_TSD (Old_TSD : S.Address; New_Tag : Tag);
   --  Entry point used to initialize the TSD of a type knowing the
   --  TSD of the direct ancestor.

   function Parent_Size (Obj : S.Address) return SSE.Storage_Count;
   --  Computes the size of field _Parent of a tagged extension object
   --  whose address is 'obj' by calling the indirectly _size function of
   --  the parent.  This function assumes that _size is always in slot 1 of
   --  the dispatch table.

   pragma Export (Ada, Parent_Size, "ada__tags__parent_size");
   --  This procedure is used in s-finimp and is thus exported manually

   procedure Register_Tag (T : Tag);
   --  Insert the Tag and its associated external_tag in a table for the
   --  sake of Internal_Tag

   procedure Set_Inheritance_Depth
     (T     : Tag;
      Value : Natural);
   --  Given a pointer to a dispatch Table, stores the value representing
   --  the depth in the inheritance tree (the second parameter). Used during
   --  elaboration of the tagged type.

   procedure Set_Prim_Op_Address
     (T        : Tag;
      Position : Positive;
      Value    : S.Address);
   --  Given a pointer to a dispatch Table (T) and a position in the
   --  dispatch Table put the address of the virtual function in it
   --  (used for overriding)

   procedure Set_TSD (T : Tag; Value : S.Address);
   --  Given a pointer T to a dispatch Table, stores the address of the record
   --  containing the Type Specific Data generated by GNAT

   procedure Set_Expanded_Name (T : Tag; Value : S.Address);
   --  Set the address of the string containing the expanded name
   --  in the Dispatch table

   procedure Set_External_Tag (T : Tag; Value : S.Address);
   --  Set the address of the string containing the external tag
   --  in the Dispatch table

   procedure Set_RC_Offset (T : Tag; Value : SSE.Storage_Offset);
   --  Sets the Offset of the implicit record controller when the object
   --  has controlled components. Set to O otherwise.

   procedure Set_Remotely_Callable (T : Tag; Value : Boolean);
   --  Set to true if the type has been declared in a context described
   --  in E.4 (18)

   DT_Prologue_Size : constant SSE.Storage_Count :=
                        SSE.Storage_Count
                          (Standard'Address_Size / S.Storage_Unit);
   --  Size of the first part of the dispatch table

   DT_Entry_Size : constant SSE.Storage_Count :=
                     SSE.Storage_Count
                       (Standard'Address_Size / S.Storage_Unit);
   --  Size of each primitive operation entry in the Dispatch Table.

   TSD_Prologue_Size : constant SSE.Storage_Count :=
                         SSE.Storage_Count
                           (6 * Standard'Address_Size / S.Storage_Unit);
   --  Size of the first part of the type specific data

   TSD_Entry_Size : constant SSE.Storage_Count :=
     SSE.Storage_Count (Standard'Address_Size / S.Storage_Unit);
   --  Size of each ancestor tag entry in the TSD

   type Address_Array is array (Natural range <>) of S.Address;

   type Dispatch_Table;
   type Tag is access all Dispatch_Table;

   type Type_Specific_Data;
   type Type_Specific_Data_Ptr is access all Type_Specific_Data;

   pragma Inline_Always (CW_Membership);
   pragma Inline_Always (Get_Expanded_Name);
   pragma Inline_Always (Get_Inheritance_Depth);
   pragma Inline_Always (Get_Prim_Op_Address);
   pragma Inline_Always (Get_RC_Offset);
   pragma Inline_Always (Get_Remotely_Callable);
   pragma Inline_Always (Get_TSD);
   pragma Inline_Always (Inherit_DT);
   pragma Inline_Always (Inherit_TSD);
   pragma Inline_Always (Register_Tag);
   pragma Inline_Always (Set_Expanded_Name);
   pragma Inline_Always (Set_External_Tag);
   pragma Inline_Always (Set_Inheritance_Depth);
   pragma Inline_Always (Set_Prim_Op_Address);
   pragma Inline_Always (Set_RC_Offset);
   pragma Inline_Always (Set_Remotely_Callable);
   pragma Inline_Always (Set_TSD);
end Ada.Tags;
