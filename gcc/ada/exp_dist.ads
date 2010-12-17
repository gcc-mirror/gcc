------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ D I S T                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2010, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains utility routines used for the generation of the
--  stubs relevant to the distribution annex.

with Namet;  use Namet;
with Snames; use Snames;
with Types;  use Types;

package Exp_Dist is

   PCS_Version_Number : constant array (PCS_Names) of Int :=
                          (Name_No_DSA      => 1,
                           Name_GARLIC_DSA  => 1,
                           Name_PolyORB_DSA => 4);
   --  PCS interface version. This is used to check for consistency between the
   --  compiler used to generate distribution stubs and the PCS implementation.
   --  It must be incremented whenever a change is made to the generated code
   --  for distribution stubs that would result in the compiler being
   --  incompatible with an older version of the PCS, or vice versa.

   procedure Add_RAST_Features (Vis_Decl : Node_Id);
   --  Build and add bodies for dereference and 'Access subprograms for a
   --  remote access to subprogram type. Vis_Decl is the declaration node for
   --  the RAS type.

   procedure Add_RACW_Features (RACW_Type : Entity_Id);
   --  Add RACW features. If the RACW and the designated type are not in the
   --  same scope, then Add_RACW_Primitive_Declarations_And_Bodies is called
   --  automatically since we do know the primitive list already.

   procedure Add_RACW_Primitive_Declarations_And_Bodies
     (Designated_Type : Entity_Id;
      Insertion_Node  : Node_Id;
      Body_Decls      : List_Id);
   --  Add primitive for the stub type, and the RPC receiver. The declarations
   --  are inserted after Insertion_Node, while the bodies are appended at the
   --  end of Body_Decls.

   procedure Remote_Types_Tagged_Full_View_Encountered
     (Full_View : Entity_Id);
   --  When a full view with a private view is encountered in a Remote_Types
   --  package and corresponds to a tagged type, then this procedure is called
   --  to generate the needed RACW features if it is needed.

   procedure RACW_Type_Is_Asynchronous (RACW_Type : Entity_Id);
   --  This subprogram must be called when it is detected that the RACW type
   --  is asynchronous.

   procedure Expand_Calling_Stubs_Bodies (Unit_Node : Node_Id);
   --  Call the expansion phase for the calling stubs. The code will be added
   --  at the end of the compilation unit, which is a package spec.

   procedure Expand_Receiving_Stubs_Bodies (Unit_Node : Node_Id);
   --  Call the expansion phase for the receiving stubs. The code will be added
   --  at the end of the compilation unit, which may be either a package spec
   --  or a package body.

   procedure Expand_All_Calls_Remote_Subprogram_Call (N : Node_Id);
   --  Rewrite a call to a subprogram located in a Remote_Call_Interface
   --  package to which the pragma All_Calls_Remote applies so that it
   --  goes through the PCS. N is either an N_Procedure_Call_Statement
   --  or an N_Function_Call node.

   procedure Build_Passive_Partition_Stub (U : Node_Id);
   --  Build stub for a shared passive package. U is the analyzed
   --  compilation unit for a package declaration.

   function Build_Subprogram_Id
     (Loc : Source_Ptr;
      E   : Entity_Id) return Node_Id;
   --  Build a literal representing the remote subprogram identifier of E

   function Copy_Specification
     (Loc         : Source_Ptr;
      Spec        : Node_Id;
      Ctrl_Type   : Entity_Id := Empty;
      New_Name    : Name_Id   := No_Name) return Node_Id;
   --  Build a subprogram specification from another one, or from an
   --  access-to-subprogram definition. If Ctrl_Type is not Empty, and any
   --  controlling formal of an anonymous access type is found, then it is
   --  replaced by an access to Ctrl_Type. If New_Name is given, then it will
   --  be used as the name for the newly created spec.

   function Corresponding_Stub_Type (RACW_Type : Entity_Id) return Entity_Id;
   --  Return the stub type associated with the given RACW type

   function Underlying_RACW_Type (RAS_Typ : Entity_Id) return Entity_Id;
   --  Given a remote access-to-subprogram type or its equivalent
   --  record type, return the RACW type generated to implement it.

   procedure Append_RACW_Bodies (Decls : List_Id; Spec_Id : Entity_Id);
   --  Append the unanalyzed subprogram bodies generated to support RACWs
   --  declared in the given package spec (RACW stream subprograms, calling
   --  stubs primitive operations) to the given list (which is expected to be
   --  the declarations list for the corresponding package body, if there is
   --  one). In the case where a body is present, the subprogram bodies must
   --  not be generated in the package spec because this would cause an
   --  incorrect attempt to freeze Taft amendment types declared in the spec.

   function Make_Transportable_Check
     (Loc  : Source_Ptr;
      Expr : Node_Id) return Node_Id;
   --  Generate a check that the given expression (an actual in a remote
   --  subprogram call, or the return value of a function in the context of
   --  a remote call) satisfies the requirements for being transportable
   --  across partitions, raising Program_Error if it does not.

   ----------------------------------------------------------------
   -- Functions for expansion of PolyORB/DSA specific attributes --
   ----------------------------------------------------------------

   function Build_From_Any_Call
     (Typ   : Entity_Id;
      N     : Node_Id;
      Decls : List_Id) return Node_Id;
   --  Build call to From_Any attribute function of type Typ with expression
   --  N as actual parameter. Decls is the declarations list for an appropriate
   --  enclosing scope of the point where the call will be inserted; if the
   --  From_Any attribute for Typ needs to be generated at this point, its
   --  declaration is appended to Decls.

   function Build_To_Any_Call
     (N     : Node_Id;
      Decls : List_Id) return Node_Id;
   --  Build call to To_Any attribute function with expression as actual
   --  parameter. Decls is the declarations list for an appropriate
   --  enclosing scope of the point where the call will be inserted; if
   --  the To_Any attribute for Typ needs to be generated at this point,
   --  its declaration is appended to Decls.

   function Build_TypeCode_Call
     (Loc   : Source_Ptr;
      Typ   : Entity_Id;
      Decls : List_Id) return Node_Id;
   --  Build call to TypeCode attribute function for Typ. Decls is the
   --  declarations list for an appropriate enclosing scope of the point
   --  where the call will be inserted; if the To_Any attribute for Typ
   --  needs to be generated at this point, its declaration is appended
   --  to Decls.

end Exp_Dist;
