------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ D I S P                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2002 Free Software Foundation, Inc.          --
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

--  This package contains routines involved in tagged types and dynamic
--  dispatching.

with Types; use Types;
package Sem_Disp is

   procedure Check_Controlling_Formals (Typ : Entity_Id; Subp : Entity_Id);
   --  Check that all controlling parameters of Subp are of type Typ,
   --  that defaults for controlling parameters are tag-indeterminate,
   --  and that the nominal subtype of the parameters and result
   --  statically match the first subtype of the controlling type.

   procedure Check_Dispatching_Call (N : Node_Id);
   --  Check if a call is a dispatching call. The subprogram is known to
   --  be a dispatching operation. The call is dispatching if all the
   --  controlling actuals are dynamically tagged. This procedure is called
   --  after overload resolution, so the call is known to be unambiguous.

   procedure Check_Dispatching_Operation (Subp, Old_Subp : Entity_Id);
   --  Add "Subp" to the list of primitive operations of the corresponding type
   --  if it has a parameter of this type and is defined at a proper place for
   --  primitive operations (new primitives are only defined in package spec,
   --  overridden operation can be defined in any scope). If Old_Subp is not
   --  Empty we are in the overriding case.

   procedure Check_Operation_From_Incomplete_Type
     (Subp : Entity_Id;
      Typ  : Entity_Id);
   --  If a primitive operation was defined for the incomplete view of the
   --  type, and the full type declaration is a derived type definition,
   --  the operation may override an inherited one.

   procedure Check_Operation_From_Private_View (Subp, Old_Subp : Entity_Id);
   --  Add "Old_Subp" to the list of primitive operations of the corresponding
   --  tagged type if it is the full view of a private tagged type. The Alias
   --  of "OldSubp" is adjusted to point to the inherited procedure of the
   --  full view because it is always this one which has to be called.

   function Find_Controlling_Arg (N : Node_Id) return Node_Id;
   --  Returns the actual controlling argument if N is dynamically tagged,
   --  and Empty if it is not dynamically tagged.

   function Find_Dispatching_Type (Subp : Entity_Id) return Entity_Id;
   --  Check whether a subprogram is dispatching, and find the tagged
   --  type of the controlling argument or arguments.

   function Is_Dynamically_Tagged (N : Node_Id) return Boolean;
   --  Used to determine whether a call is dispatching, i.e. if is an
   --  an expression of a class_Wide type, or a call to a function with
   --  controlling result where at least one operand is dynamically tagged.

   function Is_Tag_Indeterminate (N : Node_Id) return Boolean;
   --  An expression is tag-indeterminate if it is a call that dispatches
   --  on result, and all controlling operands are also indeterminate.
   --  Such a function call may inherit a tag from an enclosing call.

   procedure Propagate_Tag (Control : Node_Id; Actual : Node_Id);
   --  If a function call is tag-indeterminate,  its controlling argument is
   --  found in the context;  either an enclosing call, or the left-hand side
   --  of the enclosing assignment statement. The tag must be propagated
   --  recursively to the tag-indeterminate actuals of the call.

end Sem_Disp;
