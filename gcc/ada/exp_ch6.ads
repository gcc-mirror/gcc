------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 6                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2006, Free Software Foundation, Inc.         --
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

--  Expand routines for chapter 6 constructs

with Types; use Types;

package Exp_Ch6 is

   procedure Expand_N_Function_Call            (N : Node_Id);
   procedure Expand_N_Subprogram_Body          (N : Node_Id);
   procedure Expand_N_Subprogram_Body_Stub     (N : Node_Id);
   procedure Expand_N_Subprogram_Declaration   (N : Node_Id);
   procedure Expand_N_Procedure_Call_Statement (N : Node_Id);

   procedure Expand_Call (N : Node_Id);
   --  This procedure contains common processing for Expand_N_Function_Call,
   --  Expand_N_Procedure_Statement, and Expand_N_Entry_Call.

   function Is_Build_In_Place_Function (E : Entity_Id) return Boolean;
   --  Ada 2005 (AI-318-02): Returns True if E denotes a function or an
   --  access-to-function type whose result must be built in place; otherwise
   --  returns False. Currently this is restricted to the subset of functions
   --  whose result subtype is a constrained inherently limited type.

   function Is_Build_In_Place_Function_Call (N : Node_Id) return Boolean;
   --  Ada 2005 (AI-318-02): Returns True if N denotes a call to a function
   --  that requires handling as a build-in-place call or is a qualified
   --  expression applied to such a call; otherwise returns False.

   procedure Freeze_Subprogram (N : Node_Id);
   --  generate the appropriate expansions related to Subprogram freeze
   --  nodes (e. g. the filling of the corresponding Dispatch Table for
   --  Primitive Operations)

   procedure Make_Build_In_Place_Call_In_Allocator
     (Allocator     : Node_Id;
      Function_Call : Node_Id);
   --  Ada 2005 (AI-318-02): Handle a call to a build-in-place function that
   --  occurs as the expression initializing an allocator, by passing access
   --  to the allocated object as an additional parameter of the function call.
   --  A new access object is declared that is initialized to the result of the
   --  allocator, passed to the function, and the allocator is rewritten to
   --  refer to that access object. Function_Call must denote either an
   --  N_Function_Call node for which Is_Build_In_Place_Call is True, or else
   --  an N_Qualified_Expression node applied to such a function call.

   procedure Make_Build_In_Place_Call_In_Anonymous_Context
     (Function_Call : Node_Id);
   --  Ada 2005 (AI-318-02): Handle a call to a build-in-place function that
   --  occurs in a context that does not provide a separate object. A temporary
   --  object is created to act as the return object and an access to the
   --  temporary is passed as an additional parameter of the call. This occurs
   --  in contexts such as subprogram call actuals and object renamings.
   --  Function_Call must denote either an N_Function_Call node for which
   --  Is_Build_In_Place_Call is True, or else an N_Qualified_Expression node
   --  applied to such a function call.

   procedure Make_Build_In_Place_Call_In_Assignment
     (Assign        : Node_Id;
      Function_Call : Node_Id);
   --  Ada 2005 (AI-318-02): Handle a call to a build-in-place function that
   --  occurs as the right-hand side of an assignment statement by passing
   --  access to the left-hand sid as an additional parameter of the function
   --  call. Assign must denote a N_Assignment_Statement. Function_Call must
   --  denote either an N_Function_Call node for which Is_Build_In_Place_Call
   --  is True, or an N_Qualified_Expression node applied to such a function
   --  call.

   procedure Make_Build_In_Place_Call_In_Object_Declaration
     (Object_Decl   : Node_Id;
      Function_Call : Node_Id);
   --  Ada 2005 (AI-318-02): Handle a call to a build-in-place function that
   --  occurs as the expression initializing an object declaration by
   --  passing access to the declared object as an additional parameter of the
   --  function call. Function_Call must denote either an N_Function_Call node
   --  for which Is_Build_In_Place_Call is True, or an N_Qualified_Expression
   --  node applied to such a function call.

   procedure Register_Interface_DT_Entry
     (Related_Nod : Node_Id;
      Prim        : Entity_Id);
   --  Ada 2005 (AI-251): Register a primitive in a secondary dispatch table.
   --  Related_Nod is the node after which the expanded code will be inserted.

end Exp_Ch6;
