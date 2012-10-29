------------------------------------------------------------------------------
--                                                                          --
--                        GNAAMP COMPILER COMPONENTS                        --
--                                                                          --
--                             A D A B K E N D                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2001-2011, AdaCore                     --
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
------------------------------------------------------------------------------

--  This is the version of the Back_End package for back ends written in Ada

with Debug;
with Lib;
with Opt;      use Opt;
with Output;   use Output;
with Osint;    use Osint;
with Osint.C;  use Osint.C;
with Switch.C; use Switch.C;
with Types;    use Types;

with System.OS_Lib; use System.OS_Lib;

package body Adabkend is

   use Switch;

   -------------------
   -- Call_Back_End --
   -------------------

   procedure Call_Back_End is
   begin
      if (Opt.Verbose_Mode or Opt.Full_List)
        and then not Debug.Debug_Flag_7
      then
         Write_Eol;
         Write_Str (Product_Name);
         Write_Str (", Copyright ");
         Write_Str (Copyright_Years);
         Write_Str (" Ada Core Technologies, Inc.");
         Write_Str (" (http://www.adacore.com)");
         Write_Eol;
         Write_Eol;
      end if;

      Driver (Lib.Cunit (Types.Main_Unit));
   end Call_Back_End;

   ------------------------
   -- Scan_Compiler_Args --
   ------------------------

   procedure Scan_Compiler_Arguments is
      Output_File_Name_Seen : Boolean := False;
      --  Set to True after having scanned the file_name for switch
      --  "-gnatO file_name"

      Argument_Count : constant Integer := Arg_Count - 1;
      --  Number of arguments (excluding program name)

      Args     : Argument_List (1 .. Argument_Count);
      Next_Arg : Positive := 1;

      procedure Scan_Back_End_Switches (Switch_Chars : String);
      --  Procedure to scan out switches stored in Switch_Chars. The first
      --  character is known to be a valid switch character, and there are no
      --  blanks or other switch terminator characters in the string, so the
      --  entire string should consist of valid switch characters, except that
      --  an optional terminating NUL character is allowed.
      --
      --  If the switch is not valid, control will not return. The switches
      --  must still be scanned to skip the "-o" arguments, or internal GCC
      --  switches, which may be safely ignored by other back-ends.

      ----------------------------
      -- Scan_Back_End_Switches --
      ----------------------------

      procedure Scan_Back_End_Switches (Switch_Chars : String) is
         First : constant Positive := Switch_Chars'First + 1;
         Last  : constant Natural  := Switch_Last (Switch_Chars);

      begin
         --  Process any back end switches, returning if the switch does not
         --  affect code generation or falling through if it does, so the
         --  switch will get stored.

         if Is_Internal_GCC_Switch (Switch_Chars) then
            Next_Arg := Next_Arg + 1;
            return; -- ignore this switch

         --  Record that an object file name has been specified. The actual
         --  file name argument is picked up and saved below by the main body
         --  of Scan_Compiler_Arguments.

         elsif Switch_Chars (First .. Last) = "o" then
            if First = Last then
               Opt.Output_File_Name_Present := True;
               return;
            else
               Fail ("invalid switch: " & Switch_Chars);
            end if;

         --  Set optimization indicators appropriately. In gcc-based GNAT this
         --  is picked up from imported variables set by the gcc driver, but
         --  for compilers with non-gcc back ends we do it here to allow use
         --  of these switches by the front end. Allowed optimization switches
         --  are -Os (optimize for size), -O[0123], and -O (same as -O1).

         elsif Switch_Chars (First) = 'O' then
            if First = Last then
               Optimization_Level := 1;

            elsif Last - First = 1 then
               if Switch_Chars (Last) = 's' then
                  Optimize_Size := 1;
                  Optimization_Level := 2;  -- Consistent with gcc setting

               elsif Switch_Chars (Last) in '0' .. '3' then
                  Optimization_Level :=
                    Character'Pos (Switch_Chars (Last)) - Character'Pos ('0');

               else
                  Fail ("invalid switch: " & Switch_Chars);
               end if;

            else
               Fail ("invalid switch: " & Switch_Chars);
            end if;

         elsif Switch_Chars (First .. Last) = "quiet" then
            return; -- ignore this switch

         elsif Switch_Chars (First .. Last) = "c" then
            return; -- ignore this switch

         --  The -x switch and its language name argument will generally be
         --  ignored by non-gcc back ends (e.g. the GNAAMP back end). In any
         --  case, we save the switch and argument in the compilation switches.

         elsif Switch_Chars (First .. Last) = "x" then
            Lib.Store_Compilation_Switch (Switch_Chars);
            Next_Arg := Next_Arg + 1;

            declare
               Argv : constant String := Args (Next_Arg).all;

            begin
               if Is_Switch (Argv) then
                  Fail ("language name missing after -x");
               else
                  Lib.Store_Compilation_Switch (Argv);
               end if;
            end;

            return;

         --  Special check, the back end switch -fno-inline also sets the
         --  front end flag to entirely inhibit all inlining. So we store it
         --  and set the appropriate flag.

         elsif Switch_Chars (First .. Last) = "fno-inline" then
            Lib.Store_Compilation_Switch (Switch_Chars);
            Opt.Suppress_All_Inlining := True;
            return;

         --  Similar processing for -fpreserve-control-flow

         elsif Switch_Chars (First .. Last) = "fpreserve-control-flow" then
            Lib.Store_Compilation_Switch (Switch_Chars);
            Opt.Suppress_Control_Flow_Optimizations := True;
            return;

         --  Ignore all other back end switches

         elsif Is_Back_End_Switch (Switch_Chars) then
            null;

         --  Give error for junk switch

         else
            Fail ("invalid switch: " & Switch_Chars);
         end if;

         --  Store any other GCC switches

         Lib.Store_Compilation_Switch (Switch_Chars);
      end Scan_Back_End_Switches;

   --  Start of processing for Scan_Compiler_Args

   begin
      --  Put all the arguments in argument list Args

      for Arg in 1 .. Argument_Count loop
         declare
            Argv : String (1 .. Len_Arg (Arg));
         begin
            Fill_Arg (Argv'Address, Arg);
            Args (Arg) := new String'(Argv);
         end;
      end loop;

      --  Loop through command line arguments, storing them for later access

      while Next_Arg <= Argument_Count loop
         Look_At_Arg : declare
            Argv : constant String := Args (Next_Arg).all;

         begin
            if Argv'Length = 0 then
               Fail ("Empty argument");
            end if;

            --  If the previous switch has set the Output_File_Name_Present
            --  flag (that is we have seen a -gnatO), then the next argument
            --  is the name of the output object file.

            if Opt.Output_File_Name_Present
              and then not Output_File_Name_Seen
            then
               if Is_Switch (Argv) then
                  Fail ("Object file name missing after -gnatO");
               else
                  Set_Output_Object_File_Name (Argv);
                  Output_File_Name_Seen := True;
               end if;

               --  If the previous switch has set the Search_Directory_Present
               --  flag (that is if we have just seen -I), then the next
               --  argument is a search directory path.

            elsif Search_Directory_Present then
               if Is_Switch (Argv) then
                  Fail ("search directory missing after -I");
               else
                  Add_Src_Search_Dir (Argv);

                  --  Add directory to lib search so that back-end can take as
                  --  input ALI files if needed. Otherwise this won't have any
                  --  impact on the compiler.

                  Add_Lib_Search_Dir (Argv);

                  Search_Directory_Present := False;
               end if;

            --  If not a switch, must be a file name

            elsif not Is_Switch (Argv) then
               Add_File (Argv);

            --  Front end switch

            elsif Is_Front_End_Switch (Argv) then
               Scan_Front_End_Switches (Argv, Args, Next_Arg);

            --  All non-front-end switches are back-end switches

            else
               Scan_Back_End_Switches (Argv);
            end if;
         end Look_At_Arg;

         Next_Arg := Next_Arg + 1;
      end loop;
   end Scan_Compiler_Arguments;

end Adabkend;
