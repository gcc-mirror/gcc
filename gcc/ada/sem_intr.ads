------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ I N T R                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2023, Free Software Foundation, Inc.         --
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

--  Processing for intrinsic subprogram declarations

with Types; use Types;

package Sem_Intr is

   procedure Check_Intrinsic_Call (N : Node_Id);
   --  Perform legality check for intrinsic call N (which is either function
   --  call or a procedure call node). All the normal semantic checks have
   --  been performed already. Check_Intrinsic_Call applies any additional
   --  checks required by the fact that an intrinsic subprogram is involved.

   procedure Check_Intrinsic_Subprogram (E : Entity_Id; N : Node_Id);
   --  Special processing for pragma Import or pragma Interface when the
   --  convention is Intrinsic. E is the Entity_Id of the spec of the
   --  subprogram, and N is the second (subprogram) argument of the pragma.
   --  Check_Intrinsic_Subprogram checks that the referenced subprogram is
   --  known as an intrinsic and has an appropriate profile. If so the flag
   --  Is_Intrinsic_Subprogram is set, otherwise an error message is posted.

end Sem_Intr;
