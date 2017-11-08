------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                           S P A R K _ X R E F S                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2011-2017, Free Software Foundation, Inc.         --
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

with Output;   use Output;
with Sem_Util; use Sem_Util;

package body SPARK_Xrefs is

   ------------
   -- dspark --
   ------------

   procedure dspark is
   begin
      --  Dump SPARK cross-reference file table

      Write_Line ("SPARK Xrefs File Table");
      Write_Line ("----------------------");

      for Index in 1 .. SPARK_File_Table.Last loop
         declare
            AFR : SPARK_File_Record renames SPARK_File_Table.Table (Index);

         begin
            Write_Str ("  ");
            Write_Int (Int (Index));
            Write_Str (".  File_Num = ");
            Write_Int (Int (AFR.File_Num));
            Write_Str ("  From = ");
            Write_Int (Int (AFR.From_Scope));
            Write_Str ("  To = ");
            Write_Int (Int (AFR.To_Scope));
            Write_Eol;
         end;
      end loop;

      --  Dump SPARK cross-reference scope table

      Write_Eol;
      Write_Line ("SPARK Xrefs Scope Table");
      Write_Line ("-----------------------");

      for Index in 1 .. SPARK_Scope_Table.Last loop
         declare
            ASR : SPARK_Scope_Record renames SPARK_Scope_Table.Table (Index);

         begin
            Write_Str ("  ");
            Write_Int (Int (Index));
            Write_Str (".  File_Num = ");
            Write_Int (Int (ASR.File_Num));
            Write_Str ("  Scope_Num = ");
            Write_Int (Int (ASR.Scope_Num));
            Write_Str ("  Scope_Name = """);

            Write_Str (Unique_Name (ASR.Scope_Id));

            Write_Char ('"');
            Write_Str  ("  From = ");
            Write_Int  (Int (ASR.From_Xref));
            Write_Str  ("  To = ");
            Write_Int  (Int (ASR.To_Xref));
            Write_Eol;
         end;
      end loop;

      --  Dump SPARK cross-reference table

      Write_Eol;
      Write_Line ("SPARK Xref Table");
      Write_Line ("----------------");

      for Index in 1 .. SPARK_Xref_Table.Last loop
         declare
            AXR : SPARK_Xref_Record renames SPARK_Xref_Table.Table (Index);

         begin
            Write_Str  ("  ");
            Write_Int  (Int (Index));
            Write_Str (".  Entity_Name = """);

            Write_Str (Unique_Name (AXR.Entity));

            Write_Char ('"');
            Write_Str ("  File_Num = ");
            Write_Int (Int (AXR.File_Num));
            Write_Str ("  Scope_Num = ");
            Write_Int (Int (AXR.Scope_Num));
            Write_Str ("  Type = ");
            Write_Char (AXR.Rtype);
            Write_Eol;
         end;
      end loop;
   end dspark;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize_SPARK_Tables is
   begin
      SPARK_File_Table.Init;
      SPARK_Scope_Table.Init;
      SPARK_Xref_Table.Init;
   end Initialize_SPARK_Tables;

end SPARK_Xrefs;
