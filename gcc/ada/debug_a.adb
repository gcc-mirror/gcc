------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              D E B U G _ A                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2020, Free Software Foundation, Inc.         --
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

with Atree;   use Atree;
with Debug;   use Debug;
with Sinfo;   use Sinfo;
with Sinput;  use Sinput;
with Output;  use Output;

package body Debug_A is

   Debug_A_Depth : Natural := 0;
   --  Output for the debug A flag is preceded by a sequence of vertical bar
   --  characters corresponding to the recursion depth of the actions being
   --  recorded (analysis, expansion, resolution and evaluation of nodes)
   --  This variable records the depth.

   Max_Node_Ids : constant := 200;
   --  Maximum number of Node_Id values that get stacked

   Node_Ids : array (1 .. Max_Node_Ids) of Node_Id;
   --  A stack used to keep track of Node_Id values for setting the value of
   --  Current_Error_Node correctly. Note that if we have more than 200
   --  recursion levels, we just don't reset the right value on exit, which
   --  is not crucial, since this is only for debugging.

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Debug_Output_Astring;
   --  Outputs Debug_A_Depth number of vertical bars, used to preface messages

   -------------------
   -- Debug_A_Entry --
   -------------------

   procedure Debug_A_Entry (S : String; N : Node_Id) is
   begin
      --  Output debugging information if -gnatda flag set

      if Debug_Flag_A then
         Debug_Output_Astring;
         Write_Str (S);
         Write_Str ("Node_Id = ");
         Write_Int (Int (N));
         Write_Str ("  ");
         Write_Location (Sloc (N));
         Write_Str ("  ");
         Write_Str (Node_Kind'Image (Nkind (N)));
         Write_Eol;
      end if;

      --  Now push the new element

      --  Why is this done unconditionally???

      Debug_A_Depth := Debug_A_Depth + 1;

      if Debug_A_Depth <= Max_Node_Ids then
         Node_Ids (Debug_A_Depth) := N;
      end if;

      --  Set Current_Error_Node only if the new node has a decent Sloc
      --  value, since it is for the Sloc value that we set this anyway.
      --  If we don't have a decent Sloc value, we leave it unchanged.

      if Sloc (N) > No_Location then
         Current_Error_Node := N;
      end if;
   end Debug_A_Entry;

   ------------------
   -- Debug_A_Exit --
   ------------------

   procedure Debug_A_Exit (S : String; N : Node_Id; Comment : String) is
   begin
      Debug_A_Depth := Debug_A_Depth - 1;

      --  We look down the stack to find something with a decent Sloc. (If
      --  we find nothing, just leave it unchanged which is not so terrible)

      --  This seems nasty overhead for the normal case ???

      for J in reverse 1 .. Integer'Min (Max_Node_Ids, Debug_A_Depth) loop
         if Sloc (Node_Ids (J)) > No_Location then
            Current_Error_Node := Node_Ids (J);
            exit;
         end if;
      end loop;

      --  Output debugging information if -gnatda flag set

      if Debug_Flag_A then
         Debug_Output_Astring;
         Write_Str (S);
         Write_Str ("Node_Id = ");
         Write_Int (Int (N));
         Write_Str (Comment);
         Write_Eol;
      end if;
   end Debug_A_Exit;

   --------------------------
   -- Debug_Output_Astring --
   --------------------------

   procedure Debug_Output_Astring is
      Vbars : constant String := "|||||||||||||||||||||||||";
      --  Should be constant, removed because of GNAT 1.78 bug ???

   begin
      if Debug_A_Depth > Vbars'Length then
         for I in Vbars'Length .. Debug_A_Depth loop
            Write_Char ('|');
         end loop;

         Write_Str (Vbars);

      else
         Write_Str (Vbars (1 .. Debug_A_Depth));
      end if;
   end Debug_Output_Astring;

end Debug_A;
