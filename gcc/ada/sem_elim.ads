------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ E L I M                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1997-2004 Free Software Foundation, Inc.          --
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

--  This package contains the routines used to process the Eliminate pragma

with Types; use Types;

package Sem_Elim is

   procedure Initialize;
   --  Initialize for new main souce program

   procedure Process_Eliminate_Pragma
     (Pragma_Node         : Node_Id;
      Arg_Unit_Name       : Node_Id;
      Arg_Entity          : Node_Id;
      Arg_Parameter_Types : Node_Id;
      Arg_Result_Type     : Node_Id;
      Arg_Source_Location : Node_Id);
   --  Process eliminate pragma (given by Pragma_Node). The number of
   --  arguments has been checked, as well as possible optional identifiers,
   --  but no other checks have been made. This subprogram completes the
   --  checking, and then if the pragma is well formed, makes appropriate
   --  entries in the internal tables used to keep track of Eliminate pragmas.
   --  The other five arguments are expressions (rather than pragma argument
   --  associations) for the possible pragma arguments. A parameter that
   --  is not present is set to Empty.

   procedure Check_Eliminated (E : Entity_Id);
   --  Checks if entity E is eliminated, and if so sets the Is_Eliminated
   --  flag on the given entity.

   procedure Eliminate_Error_Msg (N : Node_Id; E : Entity_Id);
   --  Called by the back end on encouterning a call to an eliminated
   --  subprogram. N is the node for the call, and E is the entity of
   --  the subprogram being eliminated.



end Sem_Elim;
