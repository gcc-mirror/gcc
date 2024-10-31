------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ S E L                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2024, Free Software Foundation, Inc.         --
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

with Einfo;          use Einfo;
with Einfo.Entities; use Einfo.Entities;
with Nlists;         use Nlists;
with Nmake;          use Nmake;
with Rtsfind;        use Rtsfind;
with Sem_Util;       use Sem_Util;
with Snames;         use Snames;
with Stand;          use Stand;
with Tbuild;         use Tbuild;

package body Exp_Sel is

   -----------------------
   -- Build_Abort_Block --
   -----------------------

   function Build_Abort_Block
     (Loc     : Source_Ptr;
      Blk_Ent : Entity_Id;
      Blk     : Node_Id) return Node_Id
   is
   begin
      return
        Make_Block_Statement (Loc,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements =>
                New_List (
                  Make_Implicit_Label_Declaration (Loc,
                    Defining_Identifier => Blk_Ent,
                    Label_Construct     => Blk),
                  Blk),

              Exception_Handlers =>
                New_List (Build_Abort_Block_Handler (Loc))));
   end Build_Abort_Block;

   -------------------------------
   -- Build_Abort_Block_Handler --
   -------------------------------

   function Build_Abort_Block_Handler (Loc : Source_Ptr) return Node_Id is
   begin
      return Make_Implicit_Exception_Handler (Loc,
        Exception_Choices =>
          New_List (New_Occurrence_Of (Stand.Abort_Signal, Loc)),
        Statements        => New_List (Make_Null_Statement (Loc)));
   end Build_Abort_Block_Handler;

   -------------
   -- Build_B --
   -------------

   function Build_B
     (Loc   : Source_Ptr;
      Decls : List_Id) return Entity_Id
   is
      B : constant Entity_Id := Make_Temporary (Loc, 'B');
   begin
      Append_To (Decls,
        Make_Object_Declaration (Loc,
          Defining_Identifier => B,
          Object_Definition   => New_Occurrence_Of (Standard_Boolean, Loc),
          Expression          => New_Occurrence_Of (Standard_False, Loc)));
      return B;
   end Build_B;

   -------------
   -- Build_C --
   -------------

   function Build_C
     (Loc   : Source_Ptr;
      Decls : List_Id) return Entity_Id
   is
      C : constant Entity_Id := Make_Temporary (Loc, 'C');
   begin
      Append_To (Decls,
        Make_Object_Declaration (Loc,
          Defining_Identifier => C,
          Object_Definition   =>
            New_Occurrence_Of (RTE (RE_Prim_Op_Kind), Loc)));
      return C;
   end Build_C;

   -------------------------
   -- Build_Cleanup_Block --
   -------------------------

   function Build_Cleanup_Block
     (Loc       : Source_Ptr;
      Blk_Ent   : Entity_Id;
      Stmts     : List_Id;
      Clean_Ent : Entity_Id) return Node_Id
   is
      Cleanup_Block : constant Node_Id :=
                        Make_Block_Statement (Loc,
                          Identifier                 =>
                            New_Occurrence_Of (Blk_Ent, Loc),
                          Declarations               => No_List,
                          Handled_Statement_Sequence =>
                            Make_Handled_Sequence_Of_Statements (Loc,
                              Statements => Stmts),
                          Is_Asynchronous_Call_Block => True);

   begin
      Set_Entry_Cancel_Parameter (Blk_Ent, Clean_Ent);

      return Cleanup_Block;
   end Build_Cleanup_Block;

   -------------
   -- Build_K --
   -------------

   function Build_K
     (Loc   : Source_Ptr;
      Decls : List_Id;
      Obj   : Entity_Id) return Entity_Id
   is
      K        : constant Entity_Id := Make_Temporary (Loc, 'K');
      Tag_Node : constant Node_Id   :=
        Make_Attribute_Reference (Loc,
          Prefix         => New_Copy_Tree (Obj),
          Attribute_Name => Name_Tag);

   begin
      Append_To (Decls,
        Make_Object_Declaration (Loc,
          Defining_Identifier => K,
          Object_Definition   =>
            New_Occurrence_Of (RTE (RE_Tagged_Kind), Loc),
          Expression          =>
            Make_Function_Call (Loc,
              Name => New_Occurrence_Of (RTE (RE_Get_Tagged_Kind), Loc),
              Parameter_Associations => New_List (Tag_Node))));

      return K;
   end Build_K;

   -------------
   -- Build_S --
   -------------

   function Build_S
     (Loc   : Source_Ptr;
      Decls : List_Id) return Entity_Id
   is
      S : constant Entity_Id := Make_Temporary (Loc, 'S');
   begin
      Append_To (Decls,
        Make_Object_Declaration (Loc,
          Defining_Identifier => S,
          Object_Definition   => New_Occurrence_Of (Standard_Integer, Loc)));
      return S;
   end Build_S;

   ------------------------
   -- Build_S_Assignment --
   ------------------------

   function Build_S_Assignment
     (Loc      : Source_Ptr;
      S        : Entity_Id;
      Obj      : Entity_Id;
      Call_Ent : Entity_Id) return Node_Id
   is
   begin
      return
        Make_Assignment_Statement (Loc,
          Name       => New_Occurrence_Of (S, Loc),
          Expression =>
            Make_Function_Call (Loc,
              Name => New_Occurrence_Of (RTE (RE_Get_Offset_Index), Loc),
              Parameter_Associations => New_List (
                Make_Attribute_Reference (Loc,
                  Prefix => New_Copy_Tree (Obj),
                  Attribute_Name => Name_Tag),
                Make_Integer_Literal (Loc, DT_Position (Call_Ent)))));
   end Build_S_Assignment;

end Exp_Sel;
