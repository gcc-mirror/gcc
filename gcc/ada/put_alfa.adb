------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P U T _ A L F A                              --
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

with Alfa; use Alfa;

procedure Put_Alfa is
begin
   --  Loop through entries in Alfa_File_Table

   for J in 1 .. Alfa_File_Table.Last loop
      declare
         F     : Alfa_File_Record renames Alfa_File_Table.Table (J);
         Start : Scope_Index;
         Stop  : Scope_Index;

      begin
         Start := F.From_Scope;
         Stop  := F.To_Scope;

         Write_Info_Initiate ('F');
         Write_Info_Char ('D');
         Write_Info_Char (' ');
         Write_Info_Nat (F.File_Num);
         Write_Info_Char (' ');

         for N in F.File_Name'Range loop
            Write_Info_Char (F.File_Name (N));
         end loop;

         Write_Info_Terminate;

         --  Loop through scope entries for this file

         loop
            exit when Start = Stop + 1;
            pragma Assert (Start <= Stop);

            declare
               S : Alfa_Scope_Record renames Alfa_Scope_Table.Table (Start);

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

               for N in S.Scope_Name'Range loop
                  Write_Info_Char (S.Scope_Name (N));
               end loop;

               if S.Spec_File_Num /= 0 then
                  Write_Info_Char (' ');
                  Write_Info_Char ('-');
                  Write_Info_Char ('>');
                  Write_Info_Char (' ');
                  Write_Info_Nat (S.Spec_File_Num);
                  Write_Info_Char ('.');
                  Write_Info_Nat (S.Spec_Scope_Num);
               end if;

               Write_Info_Terminate;
            end;

            Start := Start + 1;
         end loop;
      end;
   end loop;

   --  Loop through entries in Alfa_File_Table

   for J in 1 .. Alfa_File_Table.Last loop
      declare
         F           : Alfa_File_Record renames Alfa_File_Table.Table (J);
         Start       : Scope_Index;
         Stop        : Scope_Index;
         File        : Nat;
         Scope       : Nat;
         Entity_Line : Nat;
         Entity_Col  : Nat;

      begin
         Start := F.From_Scope;
         Stop  := F.To_Scope;

         --  Loop through scope entries for this file

         loop
            exit when Start = Stop + 1;
            pragma Assert (Start <= Stop);

            Output_One_Scope : declare
               S : Alfa_Scope_Record renames Alfa_Scope_Table.Table (Start);

               XStart : Xref_Index;
               XStop  : Xref_Index;

            begin
               XStart := S.From_Xref;
               XStop  := S.To_Xref;

               if XStart > XStop then
                  goto Continue;
               end if;

               Write_Info_Initiate ('F');
               Write_Info_Char ('X');
               Write_Info_Char (' ');
               Write_Info_Nat (F.File_Num);
               Write_Info_Char (' ');

               for N in F.File_Name'Range loop
                  Write_Info_Char (F.File_Name (N));
               end loop;

               Write_Info_Char (' ');
               Write_Info_Char ('.');
               Write_Info_Nat (S.Scope_Num);
               Write_Info_Char (' ');

               for N in S.Scope_Name'Range loop
                  Write_Info_Char (S.Scope_Name (N));
               end loop;

               --  Default value of (0,0) is used for the special __HEAP
               --  variable so use another default value.

               Entity_Line := 0;
               Entity_Col  := 1;

               --  Loop through cross reference entries for this scope

               loop
                  exit when XStart = XStop + 1;
                  pragma Assert (XStart <= XStop);

                  Output_One_Xref : declare
                     R : Alfa_Xref_Record renames
                           Alfa_Xref_Table.Table (XStart);

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

                        for N in R.Entity_Name'Range loop
                           Write_Info_Char (R.Entity_Name (N));
                        end loop;

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

                  XStart := XStart + 1;
               end loop;

               Write_Info_Terminate;
            end Output_One_Scope;

         <<Continue>>
            Start := Start + 1;
         end loop;
      end;
   end loop;
end Put_Alfa;
