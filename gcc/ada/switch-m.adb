------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S W I T C H - M                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2001-2011, Free Software Foundation, Inc.         --
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

with Debug;    use Debug;
with Makeutl;  use Makeutl;
with Osint;    use Osint;
with Opt;      use Opt;
with Prj;      use Prj;
with Prj.Env;  use Prj.Env;
with Table;

with System.Multiprocessors; use System.Multiprocessors;

package body Switch.M is

   package Normalized_Switches is new Table.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Integer,
      Table_Low_Bound      => 1,
      Table_Initial        => 20,
      Table_Increment      => 100,
      Table_Name           => "Switch.M.Normalized_Switches");
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

      Storing      : String := Switch_Chars;
      First_Stored : Positive := Ptr + 1;
      Last_Stored  : Positive := First_Stored;

      procedure Add_Switch_Component (S : String);
      --  Add a new String_Access component in Switches. If a string equal
      --  to S is already stored in the table Normalized_Switches, use it.
      --  Otherwise add a new component to the table.

      --------------------------
      -- Add_Switch_Component --
      --------------------------

      procedure Add_Switch_Component (S : String) is
      begin
         --  If Switches is null, allocate a new array

         if Switches = null then
            Switches := new Argument_List (1 .. Initial_Number_Of_Switches);

         --  Otherwise, if Switches is full, extend it

         elsif Last = Switches'Last then
            declare
               New_Switches : constant Argument_List_Access :=
                                new Argument_List
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
         Normalized_Switches.Append (Switches (Last));
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
         C := Switch_Chars (Ptr);

         --  Processing for a switch

         case Switch_Starts_With_Gnat is

            when False =>

               --  All switches that don't start with -gnat stay as is,
               --  except -pg, -Wall, -k8, -w

               if Switch_Chars = "-pg" or else Switch_Chars = "-p" then

                  --  The gcc driver converts -pg to -p, so that is what
                  --  is stored in the ALI file.

                  Add_Switch_Component ("-p");

               elsif Switch_Chars = "-Wall" then

                  --  The gcc driver adds -gnatwa when -Wall is used

                  Add_Switch_Component ("-gnatwa");
                  Add_Switch_Component ("-Wall");

               elsif Switch_Chars = "-k8" then

                  --  The gcc driver transforms -k8 into -gnatk8

                  Add_Switch_Component ("-gnatk8");

               elsif Switch_Chars = "-w" then

                  --  The gcc driver adds -gnatws when -w is used

                  Add_Switch_Component ("-gnatws");
                  Add_Switch_Component ("-w");

               elsif Switch_Chars'Length > 6
                 and then
                   Switch_Chars (Switch_Chars'First .. Switch_Chars'First + 5)
                                                             = "--RTS="
               then
                  Add_Switch_Component (Switch_Chars);

                  --  When --RTS=mtp is used, the gcc driver adds -mrtp

                  if Switch_Chars = "--RTS=mtp" then
                     Add_Switch_Component ("-mrtp");
                  end if;

               --  Switch for universal addressing on AAMP target

               elsif Switch_Chars'Length >= 5
                 and then
                   Switch_Chars
                     (Switch_Chars'First .. Switch_Chars'First + 4) = "-univ"
               then
                  Add_Switch_Component (Switch_Chars);

               --  Switch for specifying AAMP target library

               elsif Switch_Chars'Length > 13
                 and then
                   Switch_Chars (Switch_Chars'First .. Switch_Chars'First + 12)
                     = "-aamp_target="
               then
                  Add_Switch_Component (Switch_Chars);

               --  Take only into account switches that are transmitted to
               --  gnat1 by the gcc driver and stored by gnat1 in the ALI file.

               else
                  case C is
                     when 'O' | 'W' | 'w' | 'f' | 'd' | 'g' | 'm' =>
                        Add_Switch_Component (Switch_Chars);

                     when others =>
                        null;
                  end case;
               end if;

               return;

            when True =>

               case C is

                  --  One-letter switches

                  when 'a' | 'A' | 'b' | 'B' | 'c' | 'C' | 'E' | 'f' |
                       'F' | 'g' | 'h' | 'H' | 'I' | 'L' | 'n' | 'N' |
                       'o' | 'p' | 'P' | 'q' | 'Q' | 'r' | 's' | 'S' |
                       't' | 'u' | 'U' | 'v' | 'x' | 'X' | 'Z' =>
                     Storing (First_Stored) := C;
                     Add_Switch_Component
                       (Storing (Storing'First .. First_Stored));
                     Ptr := Ptr + 1;

                  --  One-letter switches followed by a positive number

                  when 'D' | 'G' | 'j' | 'k' | 'm' | 'T' =>
                     Storing (First_Stored) := C;
                     Last_Stored := First_Stored;

                     if Ptr <= Max and then Switch_Chars (Ptr) = '=' then
                        Ptr := Ptr + 1;
                     end if;

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

                     --  Some of the gnate... switches are not stored

                     Storing (First_Stored) := 'e';
                     Ptr := Ptr + 1;

                     if Ptr > Max then
                        Last := 0;
                        return;

                     else
                        case Switch_Chars (Ptr) is

                           when 'D' =>
                              Storing (First_Stored + 1 ..
                                         First_Stored + Max - Ptr + 1) :=
                                  Switch_Chars (Ptr .. Max);
                              Add_Switch_Component
                                (Storing (Storing'First ..
                                   First_Stored + Max - Ptr + 1));
                              Ptr := Max + 1;

                           when 'G' =>
                              Ptr := Ptr + 1;
                              Add_Switch_Component ("-gnateG");

                           when 'I' =>
                              Ptr := Ptr + 1;

                              declare
                                 First : constant Positive := Ptr - 1;
                              begin
                                 if Ptr <= Max and then
                                   Switch_Chars (Ptr) = '='
                                 then
                                    Ptr := Ptr + 1;
                                 end if;

                                 while Ptr <= Max and then
                                       Switch_Chars (Ptr) in '0' .. '9'
                                 loop
                                    Ptr := Ptr + 1;
                                 end loop;

                                 Storing (First_Stored + 1 ..
                                            First_Stored + Ptr - First) :=
                                     Switch_Chars (First .. Ptr - 1);
                                 Add_Switch_Component
                                   (Storing (Storing'First ..
                                      First_Stored + Ptr - First));
                              end;

                           when 'p' =>
                              Ptr := Ptr + 1;

                              if Ptr = Max then
                                 Last := 0;
                                 return;
                              end if;

                              if Switch_Chars (Ptr) = '=' then
                                 Ptr := Ptr + 1;
                              end if;

                                 --  To normalize, always put a '=' after
                                 --  -gnatep. Because that could lengthen the
                                 --  switch string, declare a local variable.

                              declare
                                 To_Store : String (1 .. Max - Ptr + 9);
                              begin
                                 To_Store (1 .. 8) := "-gnatep=";
                                 To_Store (9 .. Max - Ptr + 9) :=
                                   Switch_Chars (Ptr .. Max);
                                 Add_Switch_Component (To_Store);
                              end;

                              return;

                           when 'S' =>
                              Ptr := Ptr + 1;
                              Add_Switch_Component ("-gnateS");

                           when others =>
                              Last := 0;
                              return;
                        end case;
                     end if;

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

                  --  -gnatl may be -gnatl=<file name>

                  when 'l' =>
                     Ptr := Ptr + 1;

                     if Ptr > Max or else Switch_Chars (Ptr) /= '=' then
                        Add_Switch_Component ("-gnatl");

                     else
                        Add_Switch_Component
                          ("-gnatl" & Switch_Chars (Ptr .. Max));
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
                             and then Switch_Chars (Ptr) = 's'
                           then
                              Last_Stored := Last_Stored + 1;
                              Storing (Last_Stored) := 's';
                              Ptr := Ptr + 1;
                           end if;
                        end if;
                     end if;

                     Add_Switch_Component
                       (Storing (Storing'First .. Last_Stored));

                  --  -gnatWx, x = 'h'. 'u', 's', 'e', '8' or 'b'

                  when 'W' =>
                     Storing (First_Stored) := 'W';
                     Ptr := Ptr + 1;

                     if Ptr <= Max then
                        case Switch_Chars (Ptr) is
                           when 'h' | 'u' | 's' | 'e' | '8' | 'b' =>
                              Storing (First_Stored + 1) := Switch_Chars (Ptr);
                              Add_Switch_Component
                                (Storing (Storing'First .. First_Stored + 1));
                              Ptr := Ptr + 1;

                           when others =>
                              Last := 0;
                              return;
                        end case;
                     end if;

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

                     --  Loop through remaining switch characters in string

                     while Ptr <= Max loop
                        C := Switch_Chars (Ptr);
                        Ptr := Ptr + 1;

                        --  -gnatyMxxx

                        if C = 'M' and then Storing (First_Stored) = 'y' then
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

                        --  --gnatx.x

                        elsif C = '.' and then Ptr <= Max then
                           Storing (First_Stored + 1) := '.';
                           Storing (First_Stored + 2) := Switch_Chars (Ptr);
                           Ptr := Ptr + 1;
                           Add_Switch_Component
                             (Storing (Storing'First .. First_Stored + 2));

                        --  All other switches are -gnatxx

                        else
                           Storing (First_Stored + 1) := C;
                           Add_Switch_Component
                             (Storing (Storing'First .. First_Stored + 1));
                        end if;
                     end loop;

                  --  -gnat95 -gnat05

                  when '0' | '9' =>
                     Last_Stored := First_Stored;
                     Storing (Last_Stored) := C;
                     Ptr := Ptr + 1;

                     if Ptr /= Max or else Switch_Chars (Ptr) /= '5' then

                        --  Invalid switch

                        Last := 0;
                        return;

                     else
                        Last_Stored := Last_Stored + 1;
                        Storing (Last_Stored) := '5';
                        Add_Switch_Component
                          (Storing (Storing'First .. Last_Stored));
                        Ptr := Ptr + 1;
                     end if;

                     --  -gnat12

                  when '1' =>
                     Last_Stored := First_Stored;
                     Storing (Last_Stored) := C;
                     Ptr := Ptr + 1;

                     if Ptr /= Max or else Switch_Chars (Ptr) /= '2' then

                        --  Invalid switch

                        Last := 0;
                        return;

                     else
                        Last_Stored := Last_Stored + 1;
                        Storing (Last_Stored) := '2';
                        Add_Switch_Component
                          (Storing (Storing'First .. Last_Stored));
                        Ptr := Ptr + 1;
                     end if;

                     --  -gnat2005 -gnat2012

                  when '2' =>
                     if Ptr + 3 /= Max then
                        Last := 0;
                        return;

                     elsif Switch_Chars (Ptr + 1 .. Ptr + 3) = "005" then
                        Last_Stored := First_Stored + 3;
                        Storing (First_Stored .. Last_Stored) := "2005";
                        Add_Switch_Component
                          (Storing (Storing'First .. Last_Stored));
                        Ptr := Max + 1;

                     elsif Switch_Chars (Ptr + 1 .. Ptr + 3) = "012" then
                        Last_Stored := First_Stored + 3;
                        Storing (First_Stored .. Last_Stored) := "2012";
                        Add_Switch_Component
                          (Storing (Storing'First .. Last_Stored));
                        Ptr := Max + 1;

                     else

                        --  Invalid switch

                        Last := 0;
                        return;

                     end if;

                  --  -gnat83

                  when '8' =>
                     Last_Stored := First_Stored;
                     Storing (Last_Stored) := '8';
                     Ptr := Ptr + 1;

                     if Ptr /= Max or else Switch_Chars (Ptr) /= '3' then

                        --  Invalid switch

                        Last := 0;
                        return;

                     else
                        Last_Stored := Last_Stored + 1;
                        Storing (Last_Stored) := '3';
                        Add_Switch_Component
                          (Storing (Storing'First .. Last_Stored));
                        Ptr := Ptr + 1;
                     end if;

                  --  Not a valid switch

                  when others =>
                     Last := 0;
                     return;

               end case;

         end case;
      end loop;
   end Normalize_Compiler_Switches;

   function Normalize_Compiler_Switches
     (Switch_Chars : String) return Argument_List
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

   procedure Scan_Make_Switches
     (Env               : in out Prj.Tree.Environment;
      Switch_Chars      : String;
      Success           : out Boolean)
   is
      Ptr : Integer          := Switch_Chars'First;
      Max : constant Integer := Switch_Chars'Last;
      C   : Character        := ' ';

   begin
      --  Assume a good switch

      Success := True;

      --  Skip past the initial character (must be the switch character)

      if Ptr = Max then
         Bad_Switch (Switch_Chars);

      else
         Ptr := Ptr + 1;
      end if;

      --  A little check, "gnat" at the start of a switch is for the compiler

      if Switch_Chars'Length >= Ptr + 3
        and then Switch_Chars (Ptr .. Ptr + 3) = "gnat"
      then
         Success := False;
         return;
      end if;

      C := Switch_Chars (Ptr);

      --  Multiple character switches

      if Switch_Chars'Length > 2 then
         if Switch_Chars = "--create-missing-dirs" then
            Setup_Projects := True;

         elsif Switch_Chars'Length > Subdirs_Option'Length
           and then
             Switch_Chars
               (Switch_Chars'First ..
                Switch_Chars'First + Subdirs_Option'Length - 1) =
                                                            Subdirs_Option
         then
            Subdirs :=
              new String'
                (Switch_Chars
                  (Switch_Chars'First + Subdirs_Option'Length ..
                   Switch_Chars'Last));

         elsif Switch_Chars = Makeutl.Unchecked_Shared_Lib_Imports then
            Opt.Unchecked_Shared_Lib_Imports := True;

         elsif Switch_Chars = Makeutl.Single_Compile_Per_Obj_Dir_Switch then
            Opt.One_Compilation_Per_Obj_Dir := True;

         elsif Switch_Chars (Ptr) = '-' then
            Bad_Switch (Switch_Chars);

         elsif Switch_Chars'Length > 3
           and then Switch_Chars (Ptr .. Ptr + 1) = "aP"
         then
            Add_Directories
              (Env.Project_Path,
               Switch_Chars (Ptr + 2 .. Switch_Chars'Last));

         elsif C = 'v' and then Switch_Chars'Length = 3 then
            Ptr := Ptr + 1;
            Verbose_Mode := True;

            case Switch_Chars (Ptr) is
               when 'l' =>
                  Verbosity_Level := Opt.Low;

               when 'm' =>
                  Verbosity_Level := Opt.Medium;

               when 'h' =>
                  Verbosity_Level := Opt.High;

               when others =>
                  Success := False;
            end case;

         elsif C = 'd' then

            --  Note: for the debug switch, the remaining characters in this
            --  switch field must all be debug flags, since all valid switch
            --  characters are also valid debug characters. This switch is not
            --  documented on purpose because it is only used by the
            --  implementors.

            --  Loop to scan out debug flags

            while Ptr < Max loop
               Ptr := Ptr + 1;
               C := Switch_Chars (Ptr);

               if C in 'a' .. 'z' or else C in 'A' .. 'Z' then
                  Set_Debug_Flag (C);
               else
                  Bad_Switch (Switch_Chars);
               end if;
            end loop;

         elsif C = 'e' then
            Ptr := Ptr + 1;

            case Switch_Chars (Ptr) is

               --  Processing for eI switch

               when 'I' =>
                  Ptr := Ptr + 1;
                  Scan_Pos (Switch_Chars, Max, Ptr, Main_Index, C);

                  if Ptr <= Max then
                     Bad_Switch (Switch_Chars);
                  end if;

               --  Processing for eL switch

               when 'L' =>
                  if Ptr /= Max then
                     Bad_Switch (Switch_Chars);

                  else
                     Follow_Links_For_Files := True;
                     Follow_Links_For_Dirs  := True;
                  end if;

               --  Processing for eS switch

               when 'S' =>
                  if Ptr /= Max then
                     Bad_Switch (Switch_Chars);

                  else
                     Commands_To_Stdout := True;
                  end if;

               when others =>
                  Bad_Switch (Switch_Chars);
            end case;

         elsif C = 'j' then
            Ptr := Ptr + 1;

            declare
               Max_Proc : Nat;

            begin
               Scan_Nat (Switch_Chars, Max, Ptr, Max_Proc, C);

               if Ptr <= Max then
                  Bad_Switch (Switch_Chars);

               else
                  if Max_Proc = 0 then
                     Max_Proc := Nat (Number_Of_CPUs);

                     if Max_Proc = 0 then
                        Max_Proc := 1;
                     end if;
                  end if;

                  Maximum_Processes := Positive (Max_Proc);
               end if;
            end;

         elsif C = 'w' and then Switch_Chars'Length = 3 then
            Ptr := Ptr + 1;

            if Switch_Chars = "-we" then
               Warning_Mode := Treat_As_Error;

            elsif Switch_Chars = "-wn" then
               Warning_Mode := Normal;

            elsif Switch_Chars = "-ws" then
               Warning_Mode  := Suppress;

            else
               Success := False;
            end if;

         else
            Success := False;
         end if;

      --  Single-character switches

      else
         Check_Switch : begin

            case C is

               when 'a' =>
                  Check_Readonly_Files := True;

               --  Processing for b switch

               when 'b' =>
                  Bind_Only  := True;
                  Make_Steps := True;

               --  Processing for B switch

               when 'B' =>
                  Build_Bind_And_Link_Full_Project := True;

               --  Processing for c switch

               when 'c' =>
                  Compile_Only := True;
                  Make_Steps   := True;

               --  Processing for C switch

               when 'C' =>
                  Opt.Create_Mapping_File := True;

               --  Processing for D switch

               when 'D' =>
                  if Object_Directory_Present then
                     Osint.Fail ("duplicate -D switch");

                  else
                     Object_Directory_Present := True;
                  end if;

               --  Processing for f switch

               when 'f' =>
                  Force_Compilations := True;

               --  Processing for F switch

               when 'F' =>
                  Full_Path_Name_For_Brief_Errors := True;

               --  Processing for h switch

               when 'h' =>
                  Usage_Requested := True;

               --  Processing for i switch

               when 'i' =>
                  In_Place_Mode := True;

               --  Processing for j switch

               when 'j' =>
                  --  -j not followed by a number is an error

                  Bad_Switch (Switch_Chars);

               --  Processing for k switch

               when 'k' =>
                  Keep_Going := True;

               --  Processing for l switch

               when 'l' =>
                  Link_Only  := True;
                  Make_Steps := True;

               --  Processing for M switch

               when 'M' =>
                  List_Dependencies := True;

               --  Processing for n switch

               when 'n' =>
                  Do_Not_Execute := True;

               --  Processing for o switch

               when 'o' =>
                  if Output_File_Name_Present then
                     Osint.Fail ("duplicate -o switch");
                  else
                     Output_File_Name_Present := True;
                  end if;

               --  Processing for p switch

               when 'p' =>
                  Setup_Projects := True;

               --  Processing for q switch

               when 'q' =>
                  Quiet_Output := True;

               --  Processing for R switch

               when 'R' =>
                  Run_Path_Option := False;

               --  Processing for s switch

               when 's' =>
                  Ptr := Ptr + 1;
                  Check_Switches := True;

               --  Processing for v switch

               when 'v' =>
                  Verbose_Mode := True;
                  Verbosity_Level := Opt.High;

                  --  Processing for x switch

               when 'x' =>
                  External_Unit_Compilation_Allowed := True;
                  Use_Include_Path_File := True;

                  --  Processing for z switch

               when 'z' =>
                  No_Main_Subprogram := True;

                  --  Any other small letter is an illegal switch

               when others =>
                  if C in 'a' .. 'z' then
                     Bad_Switch (Switch_Chars);

                  else
                     Success := False;
                  end if;

            end case;
         end Check_Switch;
      end if;
   end Scan_Make_Switches;

end Switch.M;
