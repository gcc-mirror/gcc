------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                S C A N S                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

package body Scans is

   ------------------------
   -- Restore_Scan_State --
   ------------------------

   procedure Restore_Scan_State (Saved_State : in Saved_Scan_State) is
   begin
      Scan_Ptr                 := Saved_State.Save_Scan_Ptr;
      Token                    := Saved_State.Save_Token;
      Token_Ptr                := Saved_State.Save_Token_Ptr;
      Current_Line_Start       := Saved_State.Save_Current_Line_Start;
      Start_Column             := Saved_State.Save_Start_Column;
      Checksum                 := Saved_State.Save_Checksum;
      First_Non_Blank_Location := Saved_State.Save_First_Non_Blank_Location;
      Token_Node               := Saved_State.Save_Token_Node;
      Token_Name               := Saved_State.Save_Token_Name;
      Prev_Token               := Saved_State.Save_Prev_Token;
      Prev_Token_Ptr           := Saved_State.Save_Prev_Token_Ptr;
   end Restore_Scan_State;

   ---------------------
   -- Save_Scan_State --
   ---------------------

   procedure Save_Scan_State (Saved_State : out Saved_Scan_State) is
   begin
      Saved_State.Save_Scan_Ptr                 := Scan_Ptr;
      Saved_State.Save_Token                    := Token;
      Saved_State.Save_Token_Ptr                := Token_Ptr;
      Saved_State.Save_Current_Line_Start       := Current_Line_Start;
      Saved_State.Save_Start_Column             := Start_Column;
      Saved_State.Save_Checksum                 := Checksum;
      Saved_State.Save_First_Non_Blank_Location := First_Non_Blank_Location;
      Saved_State.Save_Token_Node               := Token_Node;
      Saved_State.Save_Token_Name               := Token_Name;
      Saved_State.Save_Prev_Token               := Prev_Token;
      Saved_State.Save_Prev_Token_Ptr           := Prev_Token_Ptr;
   end Save_Scan_State;

end Scans;
