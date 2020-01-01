------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              V A L I D S W                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2001-2020, Free Software Foundation, Inc.         --
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

with Opt;    use Opt;
with Output; use Output;

package body Validsw is

   ----------------------------------
   -- Reset_Validity_Check_Options --
   ----------------------------------

   procedure Reset_Validity_Check_Options is
   begin
      Validity_Check_Components     := False;
      Validity_Check_Copies         := False;
      Validity_Check_Default        := False;
      Validity_Check_Floating_Point := False;
      Validity_Check_In_Out_Params  := False;
      Validity_Check_In_Params      := False;
      Validity_Check_Operands       := False;
      Validity_Check_Parameters     := False;
      Validity_Check_Returns        := False;
      Validity_Check_Subscripts     := False;
      Validity_Check_Tests          := False;
   end Reset_Validity_Check_Options;

   ---------------------------------
   -- Save_Validity_Check_Options --
   ---------------------------------

   procedure Save_Validity_Check_Options
     (Options : out Validity_Check_Options)
   is
      P : Natural := 0;

      procedure Add (C : Character; S : Boolean);
      --  Add given character C to string if switch S is true

      procedure Add (C : Character; S : Boolean) is
      begin
         if S then
            P := P + 1;
            Options (P) := C;
         end if;
      end Add;

   --  Start of processing for Save_Validity_Check_Options

   begin
      for K in Options'Range loop
         Options (K) := ' ';
      end loop;

      Add ('e', Validity_Check_Components);
      Add ('c', Validity_Check_Copies);
      Add ('d', Validity_Check_Default);
      Add ('f', Validity_Check_Floating_Point);
      Add ('i', Validity_Check_In_Params);
      Add ('m', Validity_Check_In_Out_Params);
      Add ('o', Validity_Check_Operands);
      Add ('p', Validity_Check_Parameters);
      Add ('r', Validity_Check_Returns);
      Add ('s', Validity_Check_Subscripts);
      Add ('t', Validity_Check_Tests);
   end Save_Validity_Check_Options;

   ----------------------------------------
   -- Set_Default_Validity_Check_Options --
   ----------------------------------------

   procedure Set_Default_Validity_Check_Options is
   begin
      Reset_Validity_Check_Options;
      Set_Validity_Check_Options ("d");
   end Set_Default_Validity_Check_Options;

   --------------------------------
   -- Set_Validity_Check_Options --
   --------------------------------

   --  Version used when no error checking is required

   procedure Set_Validity_Check_Options (Options : String) is
      OK : Boolean;
      EC : Natural;
      pragma Warnings (Off, OK);
      pragma Warnings (Off, EC);
   begin
      Set_Validity_Check_Options (Options, OK, EC);
   end Set_Validity_Check_Options;

   --  Normal version with error checking

   procedure Set_Validity_Check_Options
     (Options  : String;
      OK       : out Boolean;
      Err_Col  : out Natural)
   is
      J : Natural;
      C : Character;

   begin
      J := Options'First;
      while J <= Options'Last loop
         C := Options (J);
         J := J + 1;

         --  Turn on validity checking (gets turned off by Vn)

         Validity_Checks_On := True;

         case C is
            when 'c' =>
               Validity_Check_Copies         := True;

            when 'd' =>
               Validity_Check_Default        := True;

            when 'e' =>
               Validity_Check_Components     := True;

            when 'f' =>
               Validity_Check_Floating_Point := True;

            when 'i' =>
               Validity_Check_In_Params      := True;

            when 'm' =>
               Validity_Check_In_Out_Params  := True;

            when 'o' =>
               Validity_Check_Operands       := True;

            when 'p' =>
               Validity_Check_Parameters     := True;

            when 'r' =>
               Validity_Check_Returns        := True;

            when 's' =>
               Validity_Check_Subscripts     := True;

            when 't' =>
               Validity_Check_Tests          := True;

            when 'C' =>
               Validity_Check_Copies         := False;

            when 'D' =>
               Validity_Check_Default        := False;

            when 'E' =>
               Validity_Check_Components     := False;

            when 'F' =>
               Validity_Check_Floating_Point := False;

            when 'I' =>
               Validity_Check_In_Params      := False;

            when 'M' =>
               Validity_Check_In_Out_Params  := False;

            when 'O' =>
               Validity_Check_Operands       := False;

            when 'P' =>
               Validity_Check_Parameters     := False;

            when 'R' =>
               Validity_Check_Returns        := False;

            when 'S' =>
               Validity_Check_Subscripts     := False;

            when 'T' =>
               Validity_Check_Tests          := False;

            when 'a' =>
               Validity_Check_Components     := True;
               Validity_Check_Copies         := True;
               Validity_Check_Default        := True;
               Validity_Check_Floating_Point := True;
               Validity_Check_In_Out_Params  := True;
               Validity_Check_In_Params      := True;
               Validity_Check_Operands       := True;
               Validity_Check_Parameters     := True;
               Validity_Check_Returns        := True;
               Validity_Check_Subscripts     := True;
               Validity_Check_Tests          := True;

            when 'n' =>
               Validity_Check_Components     := False;
               Validity_Check_Copies         := False;
               Validity_Check_Default        := False;
               Validity_Check_Floating_Point := False;
               Validity_Check_In_Out_Params  := False;
               Validity_Check_In_Params      := False;
               Validity_Check_Operands       := False;
               Validity_Check_Parameters     := False;
               Validity_Check_Returns        := False;
               Validity_Check_Subscripts     := False;
               Validity_Check_Tests          := False;
               Validity_Checks_On            := False;

            when ' ' =>
               null;

            when others =>
               if Ignore_Unrecognized_VWY_Switches then
                  Write_Line ("unrecognized switch -gnatV" & C & " ignored");
               else
                  OK      := False;
                  Err_Col := J - 1;
                  return;
               end if;
         end case;
      end loop;

      OK := True;
      Err_Col := Options'Last + 1;
   end Set_Validity_Check_Options;

end Validsw;
