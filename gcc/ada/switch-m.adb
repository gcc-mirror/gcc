------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S W I T C H - M                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 2001-2002 Free Software Foundation, Inc.          --
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

with Debug;    use Debug;
with Osint;    use Osint;
with Opt;      use Opt;
with Table;

package body Switch.M is

   package Normalized_Switches is new Table.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Integer,
      Table_Low_Bound      => 1,
      Table_Initial        => 20,
      Table_Increment      => 100,
      Table_Name           => "Switch.C.Normalized_Switches");
   --  This table is used to keep the normalized switches, so that they may be
   --  reused for subsequent invocations of Normalize_Compiler_Switches with
   --  similar switches.

   Initial_Number_Of_Switches : constant := 10;

   Global_Switches : Argument_List_Access := null;
   --  Used by function Normalize_Compiler_Switches

   ---------------------------------
   -- Normalize_Compiler_Switches --
   ---------------------------------

   procedure Normalize_Compiler_Switches
     (Switch_Chars : String;
      Switches     : in out Argument_List_Access;
      Last         : out Natural)
   is
      Switch_Starts_With_Gnat : Boolean;

      Ptr : Integer := Switch_Chars'First;
      Max : constant Integer := Switch_Chars'Last;
      C   : Character := ' ';

      First_Char   : Integer := Ptr;
      Storing      : String := Switch_Chars;
      First_Stored : Positive := Ptr + 1;
      Last_Stored  : Positive := First_Stored;

      procedure Add_Switch_Component (S : String);
      --  Add a new String_Access component in Switches. If a string equal
      --  to S is already stored in the table Normalized_Switches, use it.
      --  Other wise add a new component to the table.

      --------------------------
      -- Add_Switch_Component --
      --------------------------

      procedure Add_Switch_Component (S : String) is
      begin
         --  If Switches is null, allocate a new array

         if Switches = null then
            Switches := new Argument_List (1 .. Initial_Number_Of_Switches);

         --  otherwise, if Switches is full, extend it

         elsif Last = Switches'Last then
            declare
               New_Switches : Argument_List_Access := new Argument_List
                 (1 .. Switches'Length + Switches'Length);
            begin
               New_Switches (1 .. Switches'Length) := Switches.all;
               Last := Switches'Length;
               Switches := New_Switches;
            end;
         end if;

         --  If this is the first switch, Last designates the first component
         if Last = 0 then
            Last := Switches'First;

         else
            Last := Last + 1;
         end if;

         --  Look into the table Normalized_Switches for a similar string.
         --  If one is found, put it at the added component, and return.

         for Index in 1 .. Normalized_Switches.Last loop
            if S = Normalized_Switches.Table (Index).all then
               Switches (Last) := Normalized_Switches.Table (Index);
               return;
            end if;
         end loop;

         --  No string equal to S was found in the table Normalized_Switches.
         --  Add a new component in the table.

         Switches (Last) := new String'(S);
         Normalized_Switches.Increment_Last;
         Normalized_Switches.Table (Normalized_Switches.Last) :=
           Switches (Last);
      end Add_Switch_Component;

   --  Start of processing for Normalize_Compiler_Switches

   begin
      Last := 0;

      if Ptr = Max or else Switch_Chars (Ptr) /= '-' then
         return;
      end if;

      Ptr := Ptr + 1;

      Switch_Starts_With_Gnat :=
         Ptr + 3 <= Max and then Switch_Chars (Ptr .. Ptr + 3) = "gnat";

      if Switch_Starts_With_Gnat then
         Ptr := Ptr + 4;
         First_Stored := Ptr;
      end if;

      while Ptr <= Max loop
         First_Char := Ptr;
         C := Switch_Chars (Ptr);

         --  Processing for a switch

         case Switch_Starts_With_Gnat is

            when False =>
               --  All switches that don't start with -gnat stay as is

               Add_Switch_Component (Switch_Chars);
               return;

            when True =>

               case C is

                  --  One-letter switches

                  when 'a' | 'A' | 'b' | 'c' | 'C' | 'D' | 'E' | 'f' |
                    'F' | 'g' | 'h' | 'H' | 'k' | 'l' | 'L' | 'n' | 'N' |
                    'o' | 'O' | 'p' | 'P' | 'q' | 'Q' | 'r' | 's' | 't' |
                    'u' | 'U' | 'v' | 'x' | 'X' | 'Z' =>
                     Storing (First_Stored) := C;
                     Add_Switch_Component
                       (Storing (Storing'First .. First_Stored));
                     Ptr := Ptr + 1;

                  --  One-letter switches followed by a positive number

                  when 'm' | 'T' =>
                     Storing (First_Stored) := C;
                     Last_Stored := First_Stored;

                     loop
                        Ptr := Ptr + 1;
                        exit when Ptr > Max
                          or else Switch_Chars (Ptr) not in '0' .. '9';
                        Last_Stored := Last_Stored + 1;
                        Storing (Last_Stored) := Switch_Chars (Ptr);
                     end loop;

                     Add_Switch_Component
                       (Storing (Storing'First .. Last_Stored));

                  when 'd' =>
                     Storing (First_Stored) := 'd';

                     while Ptr < Max loop
                        Ptr := Ptr + 1;
                        C := Switch_Chars (Ptr);
                        exit when C = ASCII.NUL or else C = '/'
                          or else C = '-';

                        if C in '1' .. '9' or else
                           C in 'a' .. 'z' or else
                           C in 'A' .. 'Z'
                        then
                           Storing (First_Stored + 1) := C;
                           Add_Switch_Component
                             (Storing (Storing'First .. First_Stored + 1));

                        else
                           Last := 0;
                           return;
                        end if;
                     end loop;

                     return;

                  when 'e' =>
                     --  None of the -gnate switches (-gnatec and -gnatem)
                     --  need to be store in an ALI file.

                     return;

                  when 'i' =>
                     Storing (First_Stored) := 'i';

                     Ptr := Ptr + 1;

                     if Ptr > Max then
                        Last := 0;
                        return;
                     end if;

                     C := Switch_Chars (Ptr);

                     if C in '1' .. '5'
                       or else C = '8'
                       or else C = 'p'
                       or else C = 'f'
                       or else C = 'n'
                       or else C = 'w'
                     then
                        Storing (First_Stored + 1) := C;
                        Add_Switch_Component
                          (Storing (Storing'First .. First_Stored + 1));
                        Ptr := Ptr + 1;

                     else
                        Last := 0;
                        return;
                     end if;

                  --  -gnatR may be followed by '0', '1', '2' or '3',
                  --  then by 's'

                  when 'R' =>
                     Last_Stored := First_Stored;
                     Storing (Last_Stored) := 'R';
                     Ptr := Ptr + 1;

                     if Ptr <= Max
                       and then Switch_Chars (Ptr) in '0' .. '9'
                     then
                        C := Switch_Chars (Ptr);

                        if C in '4' .. '9' then
                           Last := 0;
                           return;

                        else
                           Last_Stored := Last_Stored + 1;
                           Storing (Last_Stored) := C;
                           Ptr := Ptr + 1;

                           if Ptr <= Max
                             and then Switch_Chars (Ptr) = 's' then
                              Last_Stored := Last_Stored + 1;
                              Storing (Last_Stored) := 's';
                              Ptr := Ptr + 1;
                           end if;
                        end if;
                     end if;

                     Add_Switch_Component
                       (Storing (Storing'First .. Last_Stored));

                  --  Multiple switches

                  when 'V' | 'w' | 'y' =>
                     Storing (First_Stored) := C;
                     Ptr := Ptr + 1;

                     if Ptr > Max then
                        if C = 'y' then
                           Add_Switch_Component
                             (Storing (Storing'First .. First_Stored));

                        else
                           Last := 0;
                           return;
                        end if;
                     end if;

                     while Ptr <= Max loop
                        C := Switch_Chars (Ptr);
                        Ptr := Ptr + 1;

                        --  'w' should be skipped in -gnatw

                        if C /= 'w' or else Storing (First_Stored) /= 'w' then

                           --  -gnatyMxxx

                           if C = 'M'
                             and then Storing (First_Stored) = 'y' then
                              Last_Stored := First_Stored + 1;
                              Storing (Last_Stored) := 'M';

                              while Ptr <= Max loop
                                 C := Switch_Chars (Ptr);
                                 exit when C not in '0' .. '9';
                                 Last_Stored := Last_Stored + 1;
                                 Storing (Last_Stored) := C;
                                 Ptr := Ptr + 1;
                              end loop;

                              --  If there is no digit after -gnatyM,
                              --  the switch is invalid.

                              if Last_Stored = First_Stored + 1 then
                                 Last := 0;
                                 return;

                              else
                                 Add_Switch_Component
                                   (Storing (Storing'First .. Last_Stored));
                              end if;

                           --  All other switches are -gnatxx

                           else
                              Storing (First_Stored + 1) := C;
                              Add_Switch_Component
                                (Storing (Storing'First .. First_Stored + 1));
                           end if;
                        end if;
                     end loop;

                  --  Not a valid switch

                  when others =>
                     Last := 0;
                     return;

               end case;

         end case;
      end loop;
   end Normalize_Compiler_Switches;

   function Normalize_Compiler_Switches
     (Switch_Chars : String)
      return         Argument_List
   is
      Last : Natural;

   begin
      Normalize_Compiler_Switches (Switch_Chars, Global_Switches, Last);

      if Last = 0 then
         return (1 .. 0 => null);

      else
         return Global_Switches (Global_Switches'First .. Last);
      end if;

   end Normalize_Compiler_Switches;

   ------------------------
   -- Scan_Make_Switches --
   ------------------------

   procedure Scan_Make_Switches (Switch_Chars : String) is
      Ptr : Integer := Switch_Chars'First;
      Max : Integer := Switch_Chars'Last;
      C   : Character := ' ';

   begin
      --  Skip past the initial character (must be the switch character)

      if Ptr = Max then
         raise Bad_Switch;

      else
         Ptr := Ptr + 1;
      end if;

      --  A little check, "gnat" at the start of a switch is not allowed
      --  except for the compiler (where it was already removed)

      if Switch_Chars'Length >= Ptr + 3
        and then Switch_Chars (Ptr .. Ptr + 3) = "gnat"
      then
         Osint.Fail
           ("invalid switch: """, Switch_Chars, """ (gnat not needed here)");
      end if;

      --  Loop to scan through switches given in switch string

      while Ptr <= Max loop
         C := Switch_Chars (Ptr);

         --  Processing for a switch

         case C is

         when 'a' =>
            Ptr := Ptr + 1;
            Check_Readonly_Files := True;

         --  Processing for b switch

         when 'b' =>
            Ptr := Ptr + 1;
            Bind_Only := True;

         --  Processing for c switch

         when 'c' =>
            Ptr := Ptr + 1;
            Compile_Only := True;

         --  Processing for C switch

         when 'C' =>
            Ptr := Ptr + 1;
            Create_Mapping_File := True;

         --  Processing for d switch

         when 'd' =>

            --  Note: for the debug switch, the remaining characters in this
            --  switch field must all be debug flags, since all valid switch
            --  characters are also valid debug characters. This switch is not
            --  documented on purpose because it is only used by the
            --  implementors.

            --  Loop to scan out debug flags

            while Ptr < Max loop
               Ptr := Ptr + 1;
               C := Switch_Chars (Ptr);
               exit when C = ASCII.NUL or else C = '/' or else C = '-';

               if C in '1' .. '9' or else
                  C in 'a' .. 'z' or else
                  C in 'A' .. 'Z'
               then
                  Set_Debug_Flag (C);
               else
                  raise Bad_Switch;
               end if;
            end loop;

            --  Make sure Zero_Cost_Exceptions is set if gnatdX set. This
            --  is for backwards compatibility with old versions and usage.

            if Debug_Flag_XX then
               Zero_Cost_Exceptions_Set := True;
               Zero_Cost_Exceptions_Val := True;
            end if;

            return;

         --  Processing for f switch

         when 'f' =>
            Ptr := Ptr + 1;
            Force_Compilations := True;

         --  Processing for h switch

         when 'h' =>
            Ptr := Ptr + 1;
            Usage_Requested := True;

         --  Processing for i switch

         when 'i' =>
            Ptr := Ptr + 1;
            In_Place_Mode := True;

         --  Processing for j switch

         when 'j' =>
            Ptr := Ptr + 1;

            declare
               Max_Proc : Pos;
            begin
               Scan_Pos (Switch_Chars, Max, Ptr, Max_Proc);
               Maximum_Processes := Positive (Max_Proc);
            end;

         --  Processing for k switch

         when 'k' =>
            Ptr := Ptr + 1;
            Keep_Going := True;

         --  Processing for l switch

         when 'l' =>
            Ptr := Ptr + 1;
            Link_Only := True;

         when 'M' =>
            Ptr := Ptr + 1;
            List_Dependencies := True;

         --  Processing for n switch

         when 'n' =>
            Ptr := Ptr + 1;
            Do_Not_Execute := True;

         --  Processing for o switch

         when 'o' =>
            Ptr := Ptr + 1;

            if Output_File_Name_Present then
               raise Too_Many_Output_Files;
            else
               Output_File_Name_Present := True;
            end if;

         --  Processing for q switch

         when 'q' =>
            Ptr := Ptr + 1;
            Quiet_Output := True;

         --  Processing for s switch

         when 's' =>
            Ptr := Ptr + 1;
            Check_Switches := True;

         --  Processing for v switch

         when 'v' =>
            Ptr := Ptr + 1;
            Verbose_Mode := True;

         --  Processing for z switch

         when 'z' =>
            Ptr := Ptr + 1;
            No_Main_Subprogram := True;

         --  Ignore extra switch character

         when '/' | '-' =>
            Ptr := Ptr + 1;

         --  Anything else is an error (illegal switch character)

         when others =>
            raise Bad_Switch;

         end case;
      end loop;

   exception
      when Bad_Switch =>
         Osint.Fail ("invalid switch: ", (1 => C));

      when Bad_Switch_Value =>
         Osint.Fail ("numeric value too big for switch: ", (1 => C));

      when Missing_Switch_Value =>
         Osint.Fail ("missing numeric value for switch: ", (1 => C));

      when Too_Many_Output_Files =>
         Osint.Fail ("duplicate -o switch");

   end Scan_Make_Switches;

end Switch.M;
