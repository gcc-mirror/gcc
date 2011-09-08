------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                 A L F A                                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2011, Free Software Foundation, Inc.           --
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
with Put_Alfa;

package body Alfa is

   -----------
   -- dalfa --
   -----------

   procedure dalfa is
   begin
      --  Dump Alfa file table

      Write_Line ("Alfa File Table");
      Write_Line ("---------------");

      for Index in 1 .. Alfa_File_Table.Last loop
         declare
            AFR : Alfa_File_Record renames Alfa_File_Table.Table (Index);

         begin
            Write_Str ("  ");
            Write_Int (Int (Index));
            Write_Str (".  File_Num = ");
            Write_Int (Int (AFR.File_Num));
            Write_Str ("  File_Name = """);

            if AFR.File_Name /= null then
               Write_Str (AFR.File_Name.all);
            end if;

            Write_Char ('"');
            Write_Str ("  From = ");
            Write_Int (Int (AFR.From_Scope));
            Write_Str ("  To = ");
            Write_Int (Int (AFR.To_Scope));
            Write_Eol;
         end;
      end loop;

      --  Dump Alfa scope table

      Write_Eol;
      Write_Line ("Alfa Scope Table");
      Write_Line ("----------------");

      for Index in 1 .. Alfa_Scope_Table.Last loop
         declare
            ASR : Alfa_Scope_Record renames Alfa_Scope_Table.Table (Index);

         begin
            Write_Str ("  ");
            Write_Int (Int (Index));
            Write_Str (".  File_Num = ");
            Write_Int (Int (ASR.File_Num));
            Write_Str ("  Scope_Num = ");
            Write_Int (Int (ASR.Scope_Num));
            Write_Str ("  Scope_Name = """);

            if ASR.Scope_Name /= null then
               Write_Str (ASR.Scope_Name.all);
            end if;

            Write_Char ('"');
            Write_Str  ("  Line = ");
            Write_Int  (Int (ASR.Line));
            Write_Str  ("  Col = ");
            Write_Int  (Int (ASR.Col));
            Write_Str  ("  Type = ");
            Write_Char (ASR.Stype);
            Write_Str  ("  From = ");
            Write_Int  (Int (ASR.From_Xref));
            Write_Str  ("  To = ");
            Write_Int  (Int (ASR.To_Xref));
            Write_Str  ("  Scope_Entity = ");
            Write_Int  (Int (ASR.Scope_Entity));
            Write_Eol;
         end;
      end loop;

      --  Dump Alfa cross-reference table

      Write_Eol;
      Write_Line ("Alfa Xref Table");
      Write_Line ("---------------");

      for Index in 1 .. Alfa_Xref_Table.Last loop
         declare
            AXR : Alfa_Xref_Record renames Alfa_Xref_Table.Table (Index);

         begin
            Write_Str  ("  ");
            Write_Int  (Int (Index));
            Write_Str (".  Entity_Name = """);

            if AXR.Entity_Name /= null then
               Write_Str (AXR.Entity_Name.all);
            end if;

            Write_Char ('"');
            Write_Str ("  Entity_Line = ");
            Write_Int (Int (AXR.Entity_Line));
            Write_Str ("  Entity_Col = ");
            Write_Int (Int (AXR.Entity_Col));
            Write_Str ("  File_Num = ");
            Write_Int (Int (AXR.File_Num));
            Write_Str ("  Scope_Num = ");
            Write_Int (Int (AXR.Scope_Num));
            Write_Str ("  Line = ");
            Write_Int (Int (AXR.Line));
            Write_Str ("  Col = ");
            Write_Int (Int (AXR.Col));
            Write_Str ("  Type = ");
            Write_Char (AXR.Rtype);
            Write_Eol;
         end;
      end loop;
   end dalfa;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize_Alfa_Tables is
   begin
      Alfa_File_Table.Init;
      Alfa_Scope_Table.Init;
      Alfa_Xref_Table.Init;
   end Initialize_Alfa_Tables;

   -----------
   -- palfa --
   -----------

   procedure palfa is

      procedure Write_Info_Char (C : Character) renames Write_Char;
      --  Write one character;

      function Write_Info_Col return Positive;
      --  Return next column for writing

      procedure Write_Info_Initiate (Key : Character) renames Write_Char;
      --  Start new one and write one character;

      procedure Write_Info_Nat (N : Nat);
      --  Write value of N

      procedure Write_Info_Terminate renames Write_Eol;
      --  Terminate current line

      --------------------
      -- Write_Info_Col --
      --------------------

      function Write_Info_Col return Positive is
      begin
         return Positive (Column);
      end Write_Info_Col;

      --------------------
      -- Write_Info_Nat --
      --------------------

      procedure Write_Info_Nat (N : Nat) is
      begin
         Write_Int (N);
      end Write_Info_Nat;

      procedure Debug_Put_Alfa is new Put_Alfa;

   --  Start of processing for palfa

   begin
      Debug_Put_Alfa;
   end palfa;

end Alfa;
