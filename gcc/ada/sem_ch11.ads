------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ C H 1 1                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2021, Free Software Foundation, Inc.         --
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

with Types; use Types;
package Sem_Ch11 is
   procedure Analyze_Exception_Declaration              (N : Node_Id);
   procedure Analyze_Handled_Statements                 (N : Node_Id);
   procedure Analyze_Raise_Expression                   (N : Node_Id);
   procedure Analyze_Raise_Statement                    (N : Node_Id);
   procedure Analyze_Raise_When_Statement               (N : Node_Id);
   procedure Analyze_Raise_xxx_Error                    (N : Node_Id);

   procedure Analyze_Exception_Handlers (L : List_Id);
   --  Analyze list of exception handlers of a handled statement sequence

end Sem_Ch11;
