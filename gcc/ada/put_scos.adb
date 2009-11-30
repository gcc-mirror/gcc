------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P U T _ S C O S                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2009, Free Software Foundation, Inc.           --
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

with SCOs; use SCOs;

procedure Put_SCOs is
begin
   --  Loop through entries in SCO_Unit_Table

   for U in 1 .. SCO_Unit_Table.Last loop
      declare
         SUT : SCO_Unit_Table_Entry renames SCO_Unit_Table.Table (U);

         Start : Nat;
         Stop  : Nat;

      begin
         Start := SUT.From;
         Stop  := SUT.To;

         --  Write unit header (omitted if no SCOs are generated for this unit)

         if Start <= Stop then
            Write_Info_Initiate ('C');
            Write_Info_Char (' ');
            Write_Info_Nat (SUT.Dep_Num);
            Write_Info_Char (' ');

            for N in SUT.File_Name'Range loop
               Write_Info_Char (SUT.File_Name (N));
            end loop;

            Write_Info_Terminate;
         end if;

         --  Loop through SCO entries for this unit

         loop
            exit when Start = Stop + 1;
            pragma Assert (Start <= Stop);

            Output_SCO_Line : declare
               T : SCO_Table_Entry renames SCO_Table.Table (Start);

               procedure Output_Range (T : SCO_Table_Entry);
               --  Outputs T.From and T.To in line:col-line:col format

               ------------------
               -- Output_Range --
               ------------------

               procedure Output_Range (T : SCO_Table_Entry) is
               begin
                  Write_Info_Nat  (Nat (T.From.Line));
                  Write_Info_Char (':');
                  Write_Info_Nat  (Nat (T.From.Col));
                  Write_Info_Char ('-');
                  Write_Info_Nat  (Nat (T.To.Line));
                  Write_Info_Char (':');
                  Write_Info_Nat  (Nat (T.To.Col));
               end Output_Range;

            --  Start of processing for Output_SCO_Line

            begin
               Write_Info_Initiate ('C');
               Write_Info_Char (T.C1);

               case T.C1 is

                  --  Statements, exit

                  when 'S' | 'T' =>
                     Write_Info_Char (' ');
                     Output_Range (T);

                     --  Decision

                  when 'I' | 'E' | 'W' | 'X' =>
                     if T.C2 = ' ' then
                        Start := Start + 1;
                     end if;

                     --  Loop through table entries for this decision

                     loop
                        declare
                           T : SCO_Table_Entry renames SCO_Table.Table (Start);

                        begin
                           Write_Info_Char (' ');

                           if T.C1 = '!' or else
                              T.C1 = '^' or else
                              T.C1 = '&' or else
                              T.C1 = '|'
                           then
                              Write_Info_Char (T.C1);

                           else
                              Write_Info_Char (T.C2);
                              Output_Range (T);
                           end if;

                           exit when T.Last;
                           Start := Start + 1;
                        end;
                     end loop;

                  when others =>
                     raise Program_Error;
               end case;

               Write_Info_Terminate;
            end Output_SCO_Line;

            Start := Start + 1;
         end loop;
      end;
   end loop;
end Put_SCOs;
