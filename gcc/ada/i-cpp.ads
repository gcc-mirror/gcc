------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                       I N T E R F A C E S . C P P                        --
--                                                                          --
--                                 S p e c                                  --
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

--  Definitions for interfacing to C++ classes

--  This package corresponds to Ada.Tags but applied to tagged types which are
--  are imported from C++ and correspond exactly to a C++ Class. The code that
--  the GNAT front end generates does not know about the structure of the C++
--  dispatch table (Vtable) but always accesses it through the procedural
--  interface defined in this package, thus the implementation of this package
--  (the body) can be customized to another C++ compiler without any change in
--  the compiler code itself as long as this procedural interface is respected.
--  Note that Ada.Tags defines a very similar procedural interface to the
--  regular Ada Dispatch Table.

with System;
with System.Storage_Elements;
with Unchecked_Conversion;

package Interfaces.CPP is

   type Vtable_Ptr is private;

   function Expanded_Name (T : Vtable_Ptr) return String;
   function External_Tag  (T : Vtable_Ptr) return String;

private
   package S   renames System;
   package SSE renames System.Storage_Elements;

   type Vtable;
   type Vtable_Ptr is access all Vtable;

   type Type_Specific_Data;
   type Type_Specific_Data_Ptr is access all Type_Specific_Data;

   --  These subprograms are in the private part. They are never accessed
   --  directly except from compiler generated code, which has access to
   --  private components of packages via the Rtsfind interface.

   procedure CPP_Set_Prim_Op_Address
     (T        : Vtable_Ptr;
      Position : Positive;
      Value    : S.Address);
   --  Given a pointer to a dispatch Table (T) and a position in the
   --  dispatch Table put the address of the virtual function in it
   --  (used for overriding)

   function CPP_Get_Prim_Op_Address
     (T        : Vtable_Ptr;
      Position : Positive)
      return     S.Address;
   --  Given a pointer to a dispatch Table (T) and a position in the DT
   --  this function returns the address of the virtual function stored
   --  in it (used for dispatching calls)

   procedure CPP_Set_Inheritance_Depth
     (T     : Vtable_Ptr;
      Value : Natural);
   --  Given a pointer to a dispatch Table, stores the value representing
   --  the depth in the inheritance tree. Used during elaboration of the
   --  tagged type.

   function CPP_Get_Inheritance_Depth (T : Vtable_Ptr) return Natural;
   --  Given a pointer to a dispatch Table, retreives the value representing
   --  the depth in the inheritance tree. Used for membership.

   procedure CPP_Set_TSD (T : Vtable_Ptr; Value : S.Address);
   --  Given a pointer T to a dispatch Table, stores the address of the
   --  record containing the Type Specific Data generated by GNAT

   function CPP_Get_TSD (T : Vtable_Ptr) return S.Address;
   --  Given a pointer T to a dispatch Table, retreives the address of the
   --  record containing the Type Specific Data generated by GNAT

   CPP_DT_Prologue_Size : constant SSE.Storage_Count :=
                            SSE.Storage_Count
                              (2 * (Standard'Address_Size / S.Storage_Unit));
   --  Size of the first part of the dispatch table

   CPP_DT_Typeinfo_Ptr_Size : constant SSE.Storage_Count :=
                            SSE.Storage_Count
                              (Standard'Address_Size / System.Storage_Unit);
   --  Size of the Typeinfo_Ptr field of the Dispatch Table.

   CPP_DT_Entry_Size : constant SSE.Storage_Count :=
                         SSE.Storage_Count
                           (1 * (Standard'Address_Size / S.Storage_Unit));
   --  Size of each primitive operation entry in the Dispatch Table.

   CPP_TSD_Prologue_Size : constant SSE.Storage_Count :=
                             SSE.Storage_Count
                               (4 * (Standard'Address_Size / S.Storage_Unit));
   --  Size of the first part of the type specific data

   CPP_TSD_Entry_Size : constant SSE.Storage_Count :=
                          SSE.Storage_Count
                            (1 * (Standard'Address_Size / S.Storage_Unit));
   --  Size of each ancestor tag entry in the TSD

   procedure CPP_Inherit_DT
    (Old_T       : Vtable_Ptr;
     New_T       : Vtable_Ptr;
     Entry_Count : Natural);
   --  Entry point used to initialize the DT of a type knowing the
   --  tag of the direct ancestor and the number of primitive ops that are
   --  inherited (Entry_Count).

   procedure CPP_Inherit_TSD
     (Old_TSD : S.Address;
      New_Tag : Vtable_Ptr);
   --  Entry point used to initialize the TSD of a type knowing the
   --  TSD of the direct ancestor.

   function CPP_CW_Membership (Obj_Tag, Typ_Tag : Vtable_Ptr) return Boolean;
   --  Given the tag of an object and the tag associated to a type, return
   --  true if Obj is in Typ'Class.

   procedure CPP_Set_External_Tag (T : Vtable_Ptr; Value : S.Address);
   --  Set the address of the string containing the external tag
   --  in the Dispatch table

   function CPP_Get_External_Tag (T : Vtable_Ptr) return S.Address;
   --  Retrieve the address of a null terminated string containing
   --  the external name

   procedure CPP_Set_Expanded_Name (T : Vtable_Ptr; Value : S.Address);
   --  Set the address of the string containing the expanded name
   --  in the Dispatch table

   function CPP_Get_Expanded_Name (T : Vtable_Ptr) return S.Address;
   --  Retrieve the address of a null terminated string containing
   --  the expanded name

   procedure CPP_Set_Remotely_Callable (T : Vtable_Ptr; Value : Boolean);
   --  Since the notions of spec/body distinction and categorized packages
   --  do not exist in C, this procedure will do nothing

   function CPP_Get_Remotely_Callable (T : Vtable_Ptr) return Boolean;
   --  This function will always return True for the reason explained above

   procedure CPP_Set_RC_Offset (T : Vtable_Ptr; Value : SSE.Storage_Offset);
   --  Sets the Offset of the implicit record controller when the object
   --  has controlled components. Set to O otherwise.

   function CPP_Get_RC_Offset (T : Vtable_Ptr) return SSE.Storage_Offset;
   --  Return the Offset of the implicit record controller when the object
   --  has controlled components. O otherwise.

   function Displaced_This
    (Current_This : S.Address;
     Vptr         : Vtable_Ptr;
     Position     : Positive)
     return         S.Address;
   --  Compute the displacement on the "this" pointer in order to be
   --  compatible with MI.
   --  (used for virtual function calls)

   function TSD (T : Vtable_Ptr) return Type_Specific_Data_Ptr;
   --  This function is conceptually equivalent to Get_TSD, but
   --  returning a Type_Specific_Data_Ptr type (rather than an Address)
   --  simplifies the implementation of the other subprograms.

   type Addr_Ptr is access System.Address;

   function To_Address is
     new Unchecked_Conversion (Vtable_Ptr, System.Address);

   function To_Addr_Ptr is
      new Unchecked_Conversion (System.Address, Addr_Ptr);

   function To_Type_Specific_Data_Ptr is
     new Unchecked_Conversion (System.Address, Type_Specific_Data_Ptr);

   pragma Inline (CPP_Set_Prim_Op_Address);
   pragma Inline (CPP_Get_Prim_Op_Address);
   pragma Inline (CPP_Set_Inheritance_Depth);
   pragma Inline (CPP_Get_Inheritance_Depth);
   pragma Inline (CPP_Set_TSD);
   pragma Inline (CPP_Get_TSD);
   pragma Inline (CPP_Inherit_DT);
   pragma Inline (CPP_CW_Membership);
   pragma Inline (CPP_Set_External_Tag);
   pragma Inline (CPP_Get_External_Tag);
   pragma Inline (CPP_Set_Expanded_Name);
   pragma Inline (CPP_Get_Expanded_Name);
   pragma Inline (CPP_Set_Remotely_Callable);
   pragma Inline (CPP_Get_Remotely_Callable);
   pragma Inline (Displaced_This);
   pragma Inline (TSD);

end Interfaces.CPP;
