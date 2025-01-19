------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--               D I A G N O S T I C S . R E P O S I T O R Y                --
--                                                                          --
--                                 B o d y                                  --
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
with Diagnostics.JSON_Utils;        use Diagnostics.JSON_Utils;
with Diagnostics.Utils;             use Diagnostics.Utils;
with Diagnostics.Switch_Repository; use Diagnostics.Switch_Repository;
with Output;                        use Output;

package body Diagnostics.Repository is

   ---------------------------------
   -- Print_Diagnostic_Repository --
   ---------------------------------

   procedure Print_Diagnostic_Repository is
      First : Boolean := True;
   begin
      Write_Char ('{');
      Begin_Block;
      NL_And_Indent;

      Write_Str ("""" & "Diagnostics" & """" & ": " & "[");
      Begin_Block;

      --  Avoid printing the first switch, which is a placeholder

      for I in Diagnostic_Entries'First .. Diagnostic_Entries'Last loop

         if First then
            First := False;
         else
            Write_Char (',');
         end if;

         NL_And_Indent;

         Write_Char ('{');
         Begin_Block;
         NL_And_Indent;

         Write_String_Attribute ("Id", To_String (I));

         Write_Char (',');
         NL_And_Indent;

         if Diagnostic_Entries (I).Human_Id /= null then
            Write_String_Attribute ("Human_Id",
                                     Diagnostic_Entries (I).Human_Id.all);
         else
            Write_String_Attribute ("Human_Id", "null");
         end if;

         Write_Char (',');
         NL_And_Indent;

         if Diagnostic_Entries (I).Status = Active then
            Write_String_Attribute ("Status", "Active");
         else
            Write_String_Attribute ("Status", "Deprecated");
         end if;

         Write_Char (',');
         NL_And_Indent;

         if Diagnostic_Entries (I).Documentation /= null then
            Write_String_Attribute ("Documentation",
                                     Diagnostic_Entries (I).Documentation.all);
         else
            Write_String_Attribute ("Documentation", "null");
         end if;

         Write_Char (',');
         NL_And_Indent;

         if Diagnostic_Entries (I).Switch /= No_Switch_Id then
            Write_Char (',');
            NL_And_Indent;
            Write_String_Attribute
              ("Switch",
               Get_Switch (Diagnostic_Entries (I).Switch).Human_Id.all);
         else
            Write_String_Attribute ("Switch", "null");
         end if;

         End_Block;
         NL_And_Indent;
         Write_Char ('}');
      end loop;

      End_Block;
      NL_And_Indent;
      Write_Char (']');

      End_Block;
      NL_And_Indent;
      Write_Char ('}');

      Write_Eol;
   end Print_Diagnostic_Repository;

end Diagnostics.Repository;
