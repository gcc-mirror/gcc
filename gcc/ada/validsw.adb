------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              V A L I D S W                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.2 $
--                                                                          --
--             Copyright (C) 2001 Free Software Foundation, Inc.            --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Opt; use Opt;

package body Validsw is

   ----------------------------------
   -- Reset_Validity_Check_Options --
   ----------------------------------

   procedure Reset_Validity_Check_Options is
   begin
      Validity_Check_Copies         := False;
      Validity_Check_Default        := True;
      Validity_Check_Floating_Point := False;
      Validity_Check_In_Out_Params  := False;
      Validity_Check_In_Params      := False;
      Validity_Check_Operands       := False;
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

      Add ('n', not Validity_Check_Default);

      Add ('c', Validity_Check_Copies);
      Add ('f', Validity_Check_Floating_Point);
      Add ('i', Validity_Check_In_Params);
      Add ('m', Validity_Check_In_Out_Params);
      Add ('o', Validity_Check_Operands);
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
      Reset_Validity_Check_Options;

      J := Options'First;
      while J <= Options'Last loop
         C := Options (J);
         J := J + 1;

         case C is
            when 'c' =>
               Validity_Check_Copies         := True;

            when 'd' =>
               Validity_Check_Default        := True;

            when 'f' =>
               Validity_Check_Floating_Point := True;

            when 'i' =>
               Validity_Check_In_Params      := True;

            when 'm' =>
               Validity_Check_In_Out_Params  := True;

            when 'o' =>
               Validity_Check_Operands       := True;

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

            when 'I' =>
               Validity_Check_In_Params      := False;

            when 'F' =>
               Validity_Check_Floating_Point := False;

            when 'M' =>
               Validity_Check_In_Out_Params  := False;

            when 'O' =>
               Validity_Check_Operands       := False;

            when 'R' =>
               Validity_Check_Returns        := False;

            when 'S' =>
               Validity_Check_Subscripts     := False;

            when 'T' =>
               Validity_Check_Tests          := False;

            when 'a' =>
               Validity_Check_Copies         := True;
               Validity_Check_Default        := True;
               Validity_Check_Floating_Point := True;
               Validity_Check_In_Out_Params  := True;
               Validity_Check_In_Params      := True;
               Validity_Check_Operands       := True;
               Validity_Check_Returns        := True;
               Validity_Check_Subscripts     := True;
               Validity_Check_Tests          := True;

            when 'n' =>
               Validity_Check_Copies         := False;
               Validity_Check_Default        := False;
               Validity_Check_Floating_Point := False;
               Validity_Check_In_Out_Params  := False;
               Validity_Check_In_Params      := False;
               Validity_Check_Operands       := False;
               Validity_Check_Returns        := False;
               Validity_Check_Subscripts     := False;
               Validity_Check_Tests          := False;

            when ' ' =>
               null;

            when others =>
               OK      := False;
               Err_Col := J - 1;
               return;
         end case;
      end loop;

      Validity_Checks_On := True;
      OK := True;
      Err_Col := Options'Last + 1;
   end Set_Validity_Check_Options;

end Validsw;
