------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ S E L                               --
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
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Einfo;   use Einfo;
with Nlists;  use Nlists;
with Nmake;   use Nmake;
with Rtsfind; use Rtsfind;
with Stand;   use Stand;
with Tbuild;  use Tbuild;

package body Exp_Sel is

   -----------------------
   -- Build_Abort_Block --
   -----------------------

   function Build_Abort_Block
     (Loc         : Source_Ptr;
      Abr_Blk_Ent : Entity_Id;
      Cln_Blk_Ent : Entity_Id;
      Blk         : Node_Id) return Node_Id
   is
   begin
      return
        Make_Block_Statement (Loc,
          Identifier   => New_Reference_To (Abr_Blk_Ent, Loc),

          Declarations => No_List,

          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements =>
                New_List (
                  Make_Implicit_Label_Declaration (Loc,
                    Defining_Identifier =>
                      Cln_Blk_Ent,
                    Label_Construct =>
                      Blk),
                  Blk),

              Exception_Handlers =>
                New_List (
                  Make_Exception_Handler (Loc,
                    Exception_Choices =>
                      New_List (
                        New_Reference_To (Stand.Abort_Signal, Loc)),
                    Statements =>
                      New_List (
                        Make_Procedure_Call_Statement (Loc,
                          Name =>
                            New_Reference_To (RTE (
                              RE_Abort_Undefer), Loc),
                          Parameter_Associations => No_List))))));
   end Build_Abort_Block;

   -------------
   -- Build_B --
   -------------

   function Build_B
     (Loc   : Source_Ptr;
      Decls : List_Id) return Entity_Id
   is
      B : constant Entity_Id := Make_Defining_Identifier (Loc,
                                  Chars => New_Internal_Name ('B'));

   begin
      Append_To (Decls,
        Make_Object_Declaration (Loc,
          Defining_Identifier =>
            B,
          Object_Definition =>
            New_Reference_To (Standard_Boolean, Loc),
          Expression =>
            New_Reference_To (Standard_False, Loc)));

      return B;
   end Build_B;

   -------------
   -- Build_C --
   -------------

   function Build_C
     (Loc   : Source_Ptr;
      Decls : List_Id) return Entity_Id
   is
      C : constant Entity_Id := Make_Defining_Identifier (Loc,
                                  Chars => New_Internal_Name ('C'));

   begin
      Append_To (Decls,
        Make_Object_Declaration (Loc,
          Defining_Identifier =>
            C,
          Object_Definition =>
            New_Reference_To (RTE (RE_Prim_Op_Kind), Loc)));

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
                          Identifier   => New_Reference_To (Blk_Ent, Loc),
                          Declarations => No_List,
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
      K : constant Entity_Id := Make_Defining_Identifier (Loc,
                                  Chars => New_Internal_Name ('K'));

   begin
      Append_To (Decls,
        Make_Object_Declaration (Loc,
          Defining_Identifier => K,
          Object_Definition   =>
            New_Reference_To (RTE (RE_Tagged_Kind), Loc),
          Expression          =>
            Make_Function_Call (Loc,
              Name => New_Reference_To (RTE (RE_Get_Tagged_Kind), Loc),
              Parameter_Associations => New_List (
                Unchecked_Convert_To (RTE (RE_Tag), Obj)))));

      return K;
   end Build_K;

   -------------
   -- Build_S --
   -------------

   function Build_S
     (Loc   : Source_Ptr;
      Decls : List_Id) return Entity_Id
   is
      S : constant Entity_Id := Make_Defining_Identifier (Loc,
                                  Chars => New_Internal_Name ('S'));

   begin
      Append_To (Decls,
        Make_Object_Declaration (Loc,
          Defining_Identifier => S,
          Object_Definition   =>
            New_Reference_To (Standard_Integer, Loc)));

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
          Name => New_Reference_To (S, Loc),
          Expression =>
            Make_Function_Call (Loc,
              Name => New_Reference_To (RTE (RE_Get_Offset_Index), Loc),
              Parameter_Associations => New_List (
                Unchecked_Convert_To (RTE (RE_Tag), Obj),
                Make_Integer_Literal (Loc, DT_Position (Call_Ent)))));
   end Build_S_Assignment;

end Exp_Sel;
