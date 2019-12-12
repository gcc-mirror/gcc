------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ S E L                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2019, Free Software Foundation, Inc.         --
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

--  Routines used in Chapter 9 for the expansion of dispatching triggers in
--  select statements (Ada 2005: AI-345)

with Types; use Types;

package Exp_Sel is

   function Build_Abort_Block
     (Loc         : Source_Ptr;
      Abr_Blk_Ent : Entity_Id;
      Cln_Blk_Ent : Entity_Id;
      Blk         : Node_Id) return Node_Id;
   --  Generate:
   --    begin
   --       Blk
   --    exception
   --       when Abort_Signal => Abort_Undefer / null;
   --    end;
   --  Abr_Blk_Ent is the name of the generated block, Cln_Blk_Ent is the name
   --  of the encapsulated cleanup block, Blk is the actual block name.
   --  The exception handler code is built by Build_Abort_Block_Handler.

   function Build_Abort_Block_Handler (Loc : Source_Ptr) return Node_Id;
   --  Generate if front-end exception:
   --    when others =>
   --      Abort_Undefer;
   --  or if back-end exception:
   --    when others =>
   --      null;
   --  This is an exception handler to stop propagation of aborts, without
   --  modifying the deferal level.

   function Build_B
     (Loc   : Source_Ptr;
      Decls : List_Id) return Entity_Id;
   --  Generate:
   --    B : Boolean := False;
   --  Append the object declaration to the list and return its defining
   --  identifier.

   function Build_C
     (Loc   : Source_Ptr;
      Decls : List_Id) return Entity_Id;
   --  Generate:
   --    C : Ada.Tags.Prim_Op_Kind;
   --  Append the object declaration to the list and return its defining
   --  identifier.

   function Build_Cleanup_Block
     (Loc       : Source_Ptr;
      Blk_Ent   : Entity_Id;
      Stmts     : List_Id;
      Clean_Ent : Entity_Id) return Node_Id;
   --  Generate:
   --    declare
   --       procedure _clean is
   --       begin
   --          ...
   --       end _clean;
   --    begin
   --       Stmts
   --    at end
   --       _clean;
   --    end;
   --  Blk_Ent is the name of the generated block, Stmts is the list of
   --  encapsulated statements and Clean_Ent is the parameter to the
   --  _clean procedure.

   function Build_K
     (Loc   : Source_Ptr;
      Decls : List_Id;
      Obj   : Entity_Id) return Entity_Id;
   --  Generate
   --    K : Ada.Tags.Tagged_Kind :=
   --          Ada.Tags.Get_Tagged_Kind (Ada.Tags.Tag (Obj));
   --  where Obj is the pointer to a secondary table. Append the object
   --  declaration to the list and return its defining identifier.

   function Build_S
     (Loc  : Source_Ptr;
      Decls : List_Id) return Entity_Id;
   --  Generate:
   --    S : Integer;
   --  Append the object declaration to the list and return its defining
   --  identifier.

   function Build_S_Assignment
     (Loc      : Source_Ptr;
      S        : Entity_Id;
      Obj      : Entity_Id;
      Call_Ent : Entity_Id) return Node_Id;
   --  Generate:
   --    S := Ada.Tags.Get_Offset_Index (
   --           Ada.Tags.Tag (Obj), DT_Position (Call_Ent));
   --  where Obj is the pointer to a secondary table, Call_Ent is the entity
   --  of the dispatching call name. Return the generated assignment.

end Exp_Sel;
