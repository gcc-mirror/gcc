------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ C H 2                               --
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

with Types; use Types;

package Sem_Ch2 is

   procedure Analyze_Character_Literal           (N : Node_Id);
   procedure Analyze_Identifier                  (N : Node_Id);
   procedure Analyze_Integer_Literal             (N : Node_Id);
   procedure Analyze_Interpolated_String_Literal (N : Node_Id);
   procedure Analyze_Real_Literal                (N : Node_Id);
   procedure Analyze_String_Literal              (N : Node_Id);

private
   pragma Inline (Analyze_Character_Literal);
   pragma Inline (Analyze_Identifier);
   pragma Inline (Analyze_Integer_Literal);
   pragma Inline (Analyze_Real_Literal);
   pragma Inline (Analyze_String_Literal);

end Sem_Ch2;
