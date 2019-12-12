------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 2                               --
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

--  Expand routines for chapter 2 constructs

with Types; use Types;
package Exp_Ch2 is

   procedure Expand_N_Expanded_Name  (N : Node_Id);
   procedure Expand_N_Identifier     (N : Node_Id);
   procedure Expand_N_Real_Literal   (N : Node_Id);

   function Param_Entity (N : Node_Id) return Entity_Id;
   --  Given an expression N, determines if the expression is a reference
   --  to a formal (of a subprogram or entry), and if so returns the Id
   --  of the corresponding formal entity, otherwise returns Empty. The
   --  reason that this is in Exp_Ch2 is that it has to deal with the case
   --  where the reference is to an entry formal, and has been expanded
   --  already. Since Exp_Ch2 is in charge of the expansion, it is best
   --  suited to knowing how to detect this case. Also handles the case
   --  of references to renamings of formals.

end Exp_Ch2;
