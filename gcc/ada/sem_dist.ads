------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ D I S T                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.1 $
--                                                                          --
--          Copyright (C) 1992-2000 Free Software Foundation, Inc.          --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  Semantic processing for distribution annex facilities

with Types; use Types;

package Sem_Dist is

   procedure Add_Stub_Constructs (N : Node_Id);
   --  Create the stubs constructs for a remote call interface package
   --  specification or body or for a shared passive specification. For
   --  caller stubs, expansion takes place directly in the specification and
   --  no additional compilation unit is created.

   function Is_All_Remote_Call (N : Node_Id) return Boolean;
   --  Check whether a function or procedure call should be expanded into
   --  a remote call, because the entity is declared in a package decl that
   --  is not currently in scope, and the proper pragmas apply.

   procedure Process_Partition_Id (N : Node_Id);
   --  Replace attribute reference with call to runtime function. The result
   --  is converted to the context type, because the attribute yields a
   --  universal integer value.

   procedure Process_Remote_AST_Attribute (N : Node_Id; New_Type : Entity_Id);
   --  Given N, an access attribute reference node whose prefix is a
   --  remote subprogram, rewrite N with a call to a conversion function
   --  whose return type is New_Type.

   procedure Process_Remote_AST_Declaration (N : Node_Id);
   --  Given N, an access to subprogram type declaration node in RCI or
   --  remote types unit, build a new record (fat pointer) type declaration
   --  using the old Defining_Identifier of N and a link to the old
   --  declaration node N whose Defining_Identifier is changed.
   --  We also construct declarations of two subprograms in the unit
   --  specification which handle remote access to subprogram type
   --  (fat pointer) dereference and the unit receiver that handles
   --  remote calls (from remote access to subprogram type values.)

   function Remote_AST_E_Dereference (P : Node_Id) return Boolean;
   --  If the prefix of an explicit dereference is a record type that
   --  represent the fat pointer for an Remote access to subprogram, in
   --  the context of a call, rewrite the enclosing call node into a
   --  remote call, the first actual of which is the fat pointer. Return
   --  true if the context is correct and the transformation took place.

   function Remote_AST_I_Dereference (P : Node_Id) return Boolean;
   --  If P is a record type that represents the fat pointer for a remote
   --  access to subprogram, and P is the prefix of a call, insert an
   --  explicit dereference and perform the transformation described for
   --  the previous function.

   function Remote_AST_Null_Value
     (N    : Node_Id;
      Typ  : Entity_Id)
      return Boolean;
   --  If N is a null value and Typ a remote access to subprogram type,
   --  this function will check if null needs to be replaced with an
   --  aggregate and will return True in this case. Otherwise, it will
   --  return False.

   function Get_Subprogram_Id (E : Entity_Id) return Int;
   --  Given a subprogram defined in a RCI package, get its subprogram id
   --  which will be used for remote calls.

   function Package_Specification_Of_Scope (E : Entity_Id) return Node_Id;
   --  Return the N_Package_Specification corresponding to a scope E

end Sem_Dist;
