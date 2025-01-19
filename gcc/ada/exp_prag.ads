------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ P R A G                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
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

--  Expand routines for pragmas

with Types; use Types;

package Exp_Prag is

   procedure Expand_N_Pragma (N : Node_Id);

   procedure Expand_Pragma_Always_Terminates (Prag : Node_Id);
   --  This routine only exists for consistency with other pragmas, since
   --  Always_Terminates has no meaningful expansion.

   procedure Expand_Pragma_Contract_Cases
     (CCs     : Node_Id;
      Subp_Id : Entity_Id;
      Decls   : List_Id;
      Stmts   : in out List_Id);
   --  Given pragma Contract_Cases CCs, create the circuitry needed to evaluate
   --  case guards and trigger consequence expressions. Subp_Id is the related
   --  subprogram for which the pragma applies. Decls are the declarations of
   --  Subp_Id's body. All generated code is added to list Stmts. If Stmts is
   --  No_List on entry, a new list is created.

   procedure Expand_Pragma_Exceptional_Cases (Prag : Node_Id);
   --  Given pragma Exceptional_Cases Prag, create the circuitry needed to
   --  catch exceptions and evaluate consequence expressions.

   procedure Expand_Pragma_Exit_Cases (Prag : Node_Id);
   --  Given pragma Exit_Cases Prag, create the circuitry needed to evaluate
   --  case guards and check the exit kind (exception raised or normal return).

   procedure Expand_Pragma_Initial_Condition
     (Pack_Id : Entity_Id;
      N       : Node_Id);
   --  Verify the run-time semantics of pragma Initial_Condition when it
   --  applies to package Pack_Id. N denotes the related package spec or
   --  body.

   procedure Expand_Pragma_Subprogram_Variant
     (Prag       : Node_Id;
      Subp_Id    : Entity_Id;
      Body_Decls : List_Id);
   --  Given pragma Subprogram_Variant Prag, create the circuitry needed
   --  to evaluate variant expressions at the subprogram entry and at the
   --  recursive call. Subp_Id is the related subprogram for which the pragma
   --  applies and Body_Decls are its body declarations. On exit, the argument
   --  of Prag is replaced with a reference to procedure with checks for the
   --  variant expressions.

end Exp_Prag;
