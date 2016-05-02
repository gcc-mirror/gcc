------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       P U T _ S P A R K _ X R E F S                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2011-2016, Free Software Foundation, Inc.         --
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

with SPARK_Xrefs; use SPARK_Xrefs;

procedure Put_SPARK_Xrefs is
begin
   --  Loop through entries in SPARK_File_Table

   for J in 1 .. SPARK_File_Table.Last loop
      declare
         F : SPARK_File_Record renames SPARK_File_Table.Table (J);

      begin
         Write_Info_Initiate ('F');
         Write_Info_Char ('D');
         Write_Info_Char (' ');
         Write_Info_Nat (F.File_Num);
         Write_Info_Char (' ');

         Write_Info_Str (F.File_Name.all);

         --  If file is a subunit, print the file name for the unit

         if F.Unit_File_Name /= null then
            Write_Info_Str (" -> " & F.Unit_File_Name.all);
         end if;

         Write_Info_Terminate;

         --  Loop through scope entries for this file

         for J in F.From_Scope .. F.To_Scope loop
            declare
               S : SPARK_Scope_Record renames SPARK_Scope_Table.Table (J);

            begin
               Write_Info_Initiate ('F');
               Write_Info_Char ('S');
               Write_Info_Char (' ');
               Write_Info_Char ('.');
               Write_Info_Nat (S.Scope_Num);
               Write_Info_Char (' ');
               Write_Info_Nat (S.Line);
               Write_Info_Char (S.Stype);
               Write_Info_Nat (S.Col);
               Write_Info_Char (' ');

               pragma Assert (S.Scope_Name.all /= "");

               Write_Info_Str (S.Scope_Name.all);

               if S.Spec_File_Num /= 0 then
                  Write_Info_Str (" -> ");
                  Write_Info_Nat (S.Spec_File_Num);
                  Write_Info_Char ('.');
                  Write_Info_Nat (S.Spec_Scope_Num);
               end if;

               Write_Info_Terminate;
            end;
         end loop;
      end;
   end loop;

   --  Loop through entries in SPARK_File_Table

   for J in 1 .. SPARK_File_Table.Last loop
      declare
         F           : SPARK_File_Record renames SPARK_File_Table.Table (J);
         File        : Nat;
         Scope       : Nat;
         Entity_Line : Nat;
         Entity_Col  : Nat;

      begin
         --  Loop through scope entries for this file

         for K in F.From_Scope .. F.To_Scope loop
            Output_One_Scope : declare
               S : SPARK_Scope_Record renames SPARK_Scope_Table.Table (K);

            begin
               --  Write only non-empty tables
               if S.From_Xref <= S.To_Xref then

                  Write_Info_Initiate ('F');
                  Write_Info_Char ('X');
                  Write_Info_Char (' ');
                  Write_Info_Nat (F.File_Num);
                  Write_Info_Char (' ');

                  Write_Info_Str (F.File_Name.all);

                  Write_Info_Char (' ');
                  Write_Info_Char ('.');
                  Write_Info_Nat (S.Scope_Num);
                  Write_Info_Char (' ');

                  Write_Info_Str (S.Scope_Name.all);

                  --  Default value of (0,0) is used for the special __HEAP
                  --  variable so use another default value.

                  Entity_Line := 0;
                  Entity_Col  := 1;

                  --  Loop through cross reference entries for this scope

                  for X in S.From_Xref .. S.To_Xref loop

                     Output_One_Xref : declare
                        R : SPARK_Xref_Record renames
                          SPARK_Xref_Table.Table (X);

                     begin
                        if R.Entity_Line /= Entity_Line
                          or else R.Entity_Col /= Entity_Col
                        then
                           Write_Info_Terminate;

                           Write_Info_Initiate ('F');
                           Write_Info_Char (' ');
                           Write_Info_Nat (R.Entity_Line);
                           Write_Info_Char (R.Etype);
                           Write_Info_Nat (R.Entity_Col);
                           Write_Info_Char (' ');

                           Write_Info_Str (R.Entity_Name.all);

                           Entity_Line := R.Entity_Line;
                           Entity_Col  := R.Entity_Col;
                           File        := F.File_Num;
                           Scope       := S.Scope_Num;
                        end if;

                        if Write_Info_Col > 72 then
                           Write_Info_Terminate;
                           Write_Info_Initiate ('.');
                        end if;

                        Write_Info_Char (' ');

                        if R.File_Num /= File then
                           Write_Info_Nat (R.File_Num);
                           Write_Info_Char ('|');
                           File  := R.File_Num;
                           Scope := 0;
                        end if;

                        if R.Scope_Num /= Scope then
                           Write_Info_Char ('.');
                           Write_Info_Nat (R.Scope_Num);
                           Write_Info_Char (':');
                           Scope := R.Scope_Num;
                        end if;

                        Write_Info_Nat (R.Line);
                        Write_Info_Char (R.Rtype);
                        Write_Info_Nat (R.Col);
                     end Output_One_Xref;

                  end loop;

                  Write_Info_Terminate;
               end if;
            end Output_One_Scope;
         end loop;
      end;
   end loop;
end Put_SPARK_Xrefs;
