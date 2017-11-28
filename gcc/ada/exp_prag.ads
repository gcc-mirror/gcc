------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ P R A G                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2017, Free Software Foundation, Inc.         --
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

   procedure Expand_Pragma_Initial_Condition
     (Pack_Id : Entity_Id;
      N       : Node_Id);
   --  Verify the run-time semantics of pragma Initial_Condition when it
   --  applies to package Pack_Id. N denotes the related package spec or
   --  body.

end Exp_Prag;
