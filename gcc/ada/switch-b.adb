------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S W I T C H - B                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2001-2005, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Debug; use Debug;
with Osint; use Osint;
with Opt;   use Opt;

with System.WCh_Con; use System.WCh_Con;

package body Switch.B is

   --------------------------
   -- Scan_Binder_Switches --
   --------------------------

   procedure Scan_Binder_Switches (Switch_Chars : String) is
      Max : constant Integer := Switch_Chars'Last;
      Ptr : Integer          := Switch_Chars'First;
      C   : Character        := ' ';

   begin
      --  Skip past the initial character (must be the switch character)

      if Ptr = Max then
         Bad_Switch (C);
      else
         Ptr := Ptr + 1;
      end if;

      --  A little check, "gnat" at the start of a switch is not allowed
      --  except for the compiler

      if Switch_Chars'Last >= Ptr + 3
        and then Switch_Chars (Ptr .. Ptr + 3) = "gnat"
      then
         Osint.Fail ("invalid switch: """, Switch_Chars, """"
            & " (gnat not needed here)");
      end if;

      --  Loop to scan through switches given in switch string

      while Ptr <= Max loop
         C := Switch_Chars (Ptr);

         case C is

         --  Processing for a switch

         when 'a' =>
            Ptr := Ptr + 1;
            Use_Pragma_Linker_Constructor := True;

         --  Processing for A switch

         when 'A' =>
            Ptr := Ptr + 1;
            Ada_Bind_File := True;

         --  Processing for b switch

         when 'b' =>
            Ptr := Ptr + 1;
            Brief_Output := True;

         --  Processing for c switch

         when 'c' =>
            Ptr := Ptr + 1;

            Check_Only := True;

         --  Processing for C switch

         when 'C' =>
            Ptr := Ptr + 1;

            Ada_Bind_File := False;

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
                  Bad_Switch (C);
               end if;
            end loop;

            return;

         --  Processing for D switch

         when 'D' =>
            Ptr := Ptr + 1;
            Scan_Pos
              (Switch_Chars, Max, Ptr, Default_Sec_Stack_Size, C);

         --  Processing for e switch

         when 'e' =>
            Ptr := Ptr + 1;
            Elab_Dependency_Output := True;

         --  Processing for E switch

         when 'E' =>
            Ptr := Ptr + 1;
            Exception_Tracebacks := True;

         --  Processing for F switch

         when 'F' =>
            Ptr := Ptr + 1;
            Force_Checking_Of_Elaboration_Flags := True;

         --  Processing for g switch

         when 'g' =>
            Ptr := Ptr + 1;

            if Ptr <= Max then
               C := Switch_Chars (Ptr);

               if C in '0' .. '3' then
                  Debugger_Level :=
                    Character'Pos
                      (Switch_Chars (Ptr)) - Character'Pos ('0');
                  Ptr := Ptr + 1;
               end if;

            else
               Debugger_Level := 2;
            end if;

         --  Processing for h switch

         when 'h' =>
            Ptr := Ptr + 1;
            Usage_Requested := True;

         --  Processing for i switch

         when 'i' =>
            if Ptr = Max then
               Bad_Switch (C);
            end if;

            Ptr := Ptr + 1;
            C := Switch_Chars (Ptr);

            if C in  '1' .. '5'
              or else C = '8'
              or else C = 'p'
              or else C = 'f'
              or else C = 'n'
              or else C = 'w'
            then
               Identifier_Character_Set := C;
               Ptr := Ptr + 1;
            else
               Bad_Switch (C);
            end if;

         --  Processing for K switch

         when 'K' =>
            Ptr := Ptr + 1;
            Output_Linker_Option_List := True;

         --  Processing for l switch

         when 'l' =>
            Ptr := Ptr + 1;
            Elab_Order_Output := True;

         --  Processing for m switch

         when 'm' =>
            Ptr := Ptr + 1;
            Scan_Pos (Switch_Chars, Max, Ptr, Maximum_Errors, C);

         --  Processing for n switch

         when 'n' =>
            Ptr := Ptr + 1;
            Bind_Main_Program := False;

            --  Note: The -L option of the binder also implies -n, so
            --  any change here must also be reflected in the processing
            --  for -L that is found in Gnatbind.Scan_Bind_Arg.

         --  Processing for o switch

         when 'o' =>
            Ptr := Ptr + 1;

            if Output_File_Name_Present then
               Osint.Fail ("duplicate -o switch");

            else
               Output_File_Name_Present := True;
            end if;

         --  Processing for O switch

         when 'O' =>
            Ptr := Ptr + 1;
            Output_Object_List := True;

         --  Processing for p switch

         when 'p' =>
            Ptr := Ptr + 1;
            Pessimistic_Elab_Order := True;

         --  Processing for q switch

         when 'q' =>
            Ptr := Ptr + 1;
            Quiet_Output := True;

         --  Processing for r switch

         when 'r' =>
            Ptr := Ptr + 1;
            List_Restrictions := True;

         --  Processing for s switch

         when 's' =>
            Ptr := Ptr + 1;
            All_Sources := True;
            Check_Source_Files := True;

         --  Processing for t switch

         when 't' =>
            Ptr := Ptr + 1;
            Tolerate_Consistency_Errors := True;

         --  Processing for T switch

         when 'T' =>
            Ptr := Ptr + 1;
            Time_Slice_Set := True;
            Scan_Nat (Switch_Chars, Max, Ptr, Time_Slice_Value, C);
            Time_Slice_Value := Time_Slice_Value * 1_000;

         --  Processing for v switch

         when 'v' =>
            Ptr := Ptr + 1;
            Verbose_Mode := True;

         --  Processing for w switch

         when 'w' =>

            --  For the binder we only allow suppress/error cases

            Ptr := Ptr + 1;

            case Switch_Chars (Ptr) is

               when 'e' =>
                  Warning_Mode  := Treat_As_Error;

               when 's' =>
                  Warning_Mode  := Suppress;

               when others =>
                  Bad_Switch (C);
            end case;

            Ptr := Ptr + 1;

         --  Processing for W switch

         when 'W' =>
            Ptr := Ptr + 1;

            for J in WC_Encoding_Method loop
               if Switch_Chars (Ptr) = WC_Encoding_Letters (J) then
                  Wide_Character_Encoding_Method := J;
                  exit;

               elsif J = WC_Encoding_Method'Last then
                  Bad_Switch (C);
               end if;
            end loop;

            Upper_Half_Encoding :=
              Wide_Character_Encoding_Method in
                WC_Upper_Half_Encoding_Method;

            Ptr := Ptr + 1;

         --  Processing for x switch

         when 'x' =>
            Ptr := Ptr + 1;
            All_Sources := False;
            Check_Source_Files := False;

         --  Processing for X switch

         when 'X' =>
            Ptr := Ptr + 1;
            Scan_Pos (Switch_Chars, Max, Ptr, Default_Exit_Status, C);

         --  Processing for z switch

         when 'z' =>
            Ptr := Ptr + 1;
            No_Main_Subprogram := True;

         --  Ignore extra switch character

         when '/'  =>
            Ptr := Ptr + 1;

         --  Ignore '-' extra switch caracter, only if it isn't followed by
         --  'RTS'. If it is, then we must process the 'RTS' switch

         when '-' =>

            if Ptr + 3 <= Max and then
              Switch_Chars (Ptr + 1 .. Ptr + 3) = "RTS"
            then
               Ptr := Ptr + 1;

               if Switch_Chars (Ptr + 3) /= '=' or else
                 (Switch_Chars (Ptr + 3) = '='
                  and then Ptr + 4 > Max)
               then
                  Osint.Fail ("missing path for --RTS");
               else

                  --  valid --RTS switch
                  Opt.No_Stdinc := True;
                  Opt.RTS_Switch := True;

                  declare
                     Src_Path_Name : constant String_Ptr :=
                                       Get_RTS_Search_Dir
                                         (Switch_Chars
                                           (Ptr + 4 .. Switch_Chars'Last),
                                          Include);
                     Lib_Path_Name : constant String_Ptr :=
                                       Get_RTS_Search_Dir
                                         (Switch_Chars
                                           (Ptr + 4 .. Switch_Chars'Last),
                                          Objects);

                  begin
                     if Src_Path_Name /= null and then
                       Lib_Path_Name /= null
                     then
                        --  Set the RTS_*_Path_Name variables, so that the
                        --  correct directories will be set when
                        --  Osint.Add_Default_Search_Dirs will be called later.

                        RTS_Src_Path_Name := Src_Path_Name;
                        RTS_Lib_Path_Name := Lib_Path_Name;

                        --  We can exit as there cannot be another switch
                        --  after --RTS

                        exit;

                     elsif  Src_Path_Name = null
                       and then Lib_Path_Name = null
                     then
                        Osint.Fail ("RTS path not valid: missing " &
                                    "adainclude and adalib directories");
                     elsif Src_Path_Name = null then
                        Osint.Fail ("RTS path not valid: missing " &
                                    "adainclude directory");
                     elsif  Lib_Path_Name = null then
                        Osint.Fail ("RTS path not valid: missing " &
                                    "adalib directory");
                     end if;
                  end;
               end if;

            else
               Ptr := Ptr + 1;
            end if;

         --  Anything else is an error (illegal switch character)

         when others =>
            Bad_Switch (C);
         end case;
      end loop;
   end Scan_Binder_Switches;

end Switch.B;
