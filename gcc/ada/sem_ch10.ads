------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ C H 1 0                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.1.16.1 $
--                                                                          --
--          Copyright (C) 1992-2001 Free Software Foundation, Inc.          --
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

with Types; use Types;
package Sem_Ch10 is
   procedure Analyze_Compilation_Unit                   (N : Node_Id);
   procedure Analyze_With_Clause                        (N : Node_Id);
   procedure Analyze_With_Type_Clause                   (N : Node_Id);
   procedure Analyze_Subprogram_Body_Stub               (N : Node_Id);
   procedure Analyze_Package_Body_Stub                  (N : Node_Id);
   procedure Analyze_Task_Body_Stub                     (N : Node_Id);
   procedure Analyze_Protected_Body_Stub                (N : Node_Id);
   procedure Analyze_Subunit                            (N : Node_Id);

   procedure Install_Context (N : Node_Id);
   --  Installs the entities from the context clause of the given compilation
   --  unit into the visibility chains. This is done before analyzing a unit.
   --  For a child unit, install context of parents as well.

   procedure Remove_Context (N : Node_Id);
   --  Removes the entities from the context clause of the given compilation
   --  unit from the visibility chains. This is done on exit from a unit as
   --  part of cleaning up the visibility chains for the caller. A special
   --  case is that the call from the Main_Unit can be ignored, since at the
   --  end of the main unit the visibility table won't be needed in any case.
   --  For a child unit, remove parents and their context as well.

   procedure Load_Needed_Body (N : Node_Id; OK : out Boolean);
   --  Load and analyze the body of a context unit that is generic, or
   --  that contains generic units or inlined units. The body becomes
   --  part of the semantic dependency set of the unit that needs it.
   --  The returned result in OK is True if the load is successful,
   --  and False if the requested file cannot be found.

end Sem_Ch10;
