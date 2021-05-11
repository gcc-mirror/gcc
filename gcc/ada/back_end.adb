------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                            B A C K _ E N D                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2021, Free Software Foundation, Inc.         --
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

--  This is the version of the Back_End package for GCC back ends

with Atree;    use Atree;
with Debug;    use Debug;
with Elists;   use Elists;
with Errout;   use Errout;
with Lib;      use Lib;
with Osint;    use Osint;
with Opt;      use Opt;
with Osint.C;  use Osint.C;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Stand;    use Stand;
with Sinput;   use Sinput;
with Stringt;  use Stringt;
with Switch;   use Switch;
with Switch.C; use Switch.C;
with System;   use System;
with Types;    use Types;

with System.OS_Lib; use System.OS_Lib;

package body Back_End is

   type Arg_Array is array (Nat) of Big_String_Ptr;
   type Arg_Array_Ptr is access Arg_Array;
   --  Types to access compiler arguments

   flag_stack_check : Int;
   pragma Import (C, flag_stack_check);
   --  Indicates if stack checking is enabled, imported from misc.c

   save_argc : Nat;
   pragma Import (C, save_argc);
   --  Saved value of argc (number of arguments), imported from misc.c

   save_argv : Arg_Array_Ptr;
   pragma Import (C, save_argv);
   --  Saved value of argv (argument pointers), imported from misc.c

   function Len_Arg (Arg : Pos) return Nat;
   --  Determine length of argument number Arg on original gnat1 command line

   -------------------
   -- Call_Back_End --
   -------------------

   procedure Call_Back_End (Mode : Back_End_Mode_Type) is

      --  The Source_File_Record type has a lot of components that are
      --  meaningless to the back end, so a new record type is created
      --  here to contain the needed information for each file.

      type File_Info_Type is record
         File_Name        : File_Name_Type;
         Instance         : Instance_Id;
         Num_Source_Lines : Nat;
      end record;

      File_Info_Array : array (1 .. Last_Source_File) of File_Info_Type;

      procedure gigi
        (gnat_root                     : Int;
         max_gnat_node                 : Int;
         number_name                   : Nat;
         node_offsets_ptr              : Address;
         slots_ptr                     : Address;

         next_node_ptr                 : Address;
         prev_node_ptr                 : Address;
         elists_ptr                    : Address;
         elmts_ptr                     : Address;

         strings_ptr                   : Address;
         string_chars_ptr              : Address;
         list_headers_ptr              : Address;
         number_file                   : Nat;

         file_info_ptr                 : Address;
         gigi_standard_boolean         : Entity_Id;
         gigi_standard_integer         : Entity_Id;
         gigi_standard_character       : Entity_Id;
         gigi_standard_long_long_float : Entity_Id;
         gigi_standard_exception_type  : Entity_Id;
         gigi_operating_mode           : Back_End_Mode_Type);

      pragma Import (C, gigi);

   begin
      --  Skip call if in -gnatdH mode

      if Debug_Flag_HH then
         return;
      end if;

      --  The back end needs to know the maximum line number that can appear
      --  in a Sloc, in other words the maximum logical line number.

      for J in 1 .. Last_Source_File loop
         File_Info_Array (J).File_Name        := Full_Debug_Name (J);
         File_Info_Array (J).Instance         := Instance (J);
         File_Info_Array (J).Num_Source_Lines :=
           Nat (Physical_To_Logical (Last_Source_Line (J), J));
      end loop;

      --  Deal with case of generating SCIL, we should not be here unless
      --  debugging CodePeer mode in GNAT.

      if Generate_SCIL then
         Error_Msg_N ("'S'C'I'L generation not available", Cunit (Main_Unit));

         if CodePeer_Mode
           or else (Mode /= Generate_Object
                     and then not Back_Annotate_Rep_Info)
         then
            return;
         end if;
      end if;

      --  We should be here in GNATprove mode only when debugging GNAT. Do not
      --  call gigi in that case, as it is not prepared to handle the special
      --  form of the tree obtained in GNATprove mode.

      if GNATprove_Mode then
         return;
      end if;

      --  The actual call to the back end

      gigi
        (gnat_root          => Int (Cunit (Main_Unit)),
         max_gnat_node      => Int (Last_Node_Id - First_Node_Id + 1),
         number_name        => Name_Entries_Count,
         node_offsets_ptr   => Node_Offsets_Address,
         slots_ptr          => Slots_Address,

         next_node_ptr      => Next_Node_Address,
         prev_node_ptr      => Prev_Node_Address,
         elists_ptr         => Elists_Address,
         elmts_ptr          => Elmts_Address,

         strings_ptr        => Strings_Address,
         string_chars_ptr   => String_Chars_Address,
         list_headers_ptr   => Lists_Address,
         number_file        => Num_Source_Files,

         file_info_ptr                 => File_Info_Array'Address,
         gigi_standard_boolean         => Standard_Boolean,
         gigi_standard_integer         => Standard_Integer,
         gigi_standard_character       => Standard_Character,
         gigi_standard_long_long_float => Standard_Long_Long_Float,
         gigi_standard_exception_type  => Standard_Exception_Type,
         gigi_operating_mode           => Mode);
   end Call_Back_End;

   -------------------------------
   -- Gen_Or_Update_Object_File --
   -------------------------------

   procedure Gen_Or_Update_Object_File is
   begin
      null;
   end Gen_Or_Update_Object_File;

   -------------
   -- Len_Arg --
   -------------

   function Len_Arg (Arg : Pos) return Nat is
   begin
      for J in 1 .. Nat'Last loop
         if save_argv (Arg).all (Natural (J)) = ASCII.NUL then
            return J - 1;
         end if;
      end loop;

      raise Program_Error;
   end Len_Arg;

   -----------------------------
   -- Scan_Compiler_Arguments --
   -----------------------------

   procedure Scan_Compiler_Arguments is
      Next_Arg : Positive;
      --  Next argument to be scanned

      Arg_Count : constant Natural := Natural (save_argc - 1);
      Args      : Argument_List (1 .. Arg_Count);

      Output_File_Name_Seen : Boolean := False;
      --  Set to True after having scanned file_name for switch "-gnatO file"

      procedure Scan_Back_End_Switches (Switch_Chars : String);
      --  Procedure to scan out switches stored in Switch_Chars. The first
      --  character is known to be a valid switch character, and there are no
      --  blanks or other switch terminator characters in the string, so the
      --  entire string should consist of valid switch characters, except that
      --  an optional terminating NUL character is allowed.
      --
      --  Back end switches have already been checked and processed by GCC in
      --  toplev.c, so no errors can occur and control will always return. The
      --  switches must still be scanned to skip "-o" or internal GCC switches
      --  with their argument.

      ----------------------------
      -- Scan_Back_End_Switches --
      ----------------------------

      procedure Scan_Back_End_Switches (Switch_Chars : String) is
         First : constant Positive := Switch_Chars'First + 1;
         Last  : constant Natural  := Switch_Last (Switch_Chars);

      begin
         --  Skip -o or internal GCC switches together with their argument

         if Switch_Chars (First .. Last) = "o"
           or else Is_Internal_GCC_Switch (Switch_Chars)
         then
            Next_Arg := Next_Arg + 1;

         --  Store -G xxx as -Gxxx and go directly to the next argument

         elsif Switch_Chars (First .. Last) = "G" then
            Next_Arg := Next_Arg + 1;

            --  Should never get there with -G not followed by an argument,
            --  but use defensive code nonetheless. Store as -Gxxx to avoid
            --  storing parameters in ALI files that might create confusion.

            if Next_Arg <= Args'Last then
               Store_Compilation_Switch (Switch_Chars & Args (Next_Arg).all);
            end if;

         --  Do not record -quiet switch

         elsif Switch_Chars (First .. Last) = "quiet" then
            null;

         --  Store any other GCC switches. Also do special processing for some
         --  specific switches that the Ada front-end knows about.

         else
            Store_Compilation_Switch (Switch_Chars);

            --  For gcc back ends, -fno-inline disables Inline pragmas only,
            --  not Inline_Always to remain consistent with the always_inline
            --  attribute behavior.

            if Switch_Chars (First .. Last) = "fno-inline" then
               Opt.Disable_FE_Inline := True;

            --  Back end switch -fpreserve-control-flow also sets the front end
            --  flag that inhibits improper control flow transformations.

            elsif Switch_Chars (First .. Last) = "fpreserve-control-flow" then
               Opt.Suppress_Control_Flow_Optimizations := True;

            --  Back end switch -fdump-scos, which exists primarily for C, is
            --  also accepted for Ada as a synonym of -gnateS.

            elsif Switch_Chars (First .. Last) = "fdump-scos" then
               Opt.Generate_SCO := True;
               Opt.Generate_SCO_Instance_Table := True;

            elsif Switch_Chars (First) = 'g' then
               Debugger_Level := 2;

               if First < Last then
                  case Switch_Chars (First + 1) is
                     when '0' =>
                        Debugger_Level := 0;
                     when '1' =>
                        Debugger_Level := 1;
                     when '2' =>
                        Debugger_Level := 2;
                     when '3' =>
                        Debugger_Level := 3;
                     when others =>
                        null;
                  end case;
               end if;
            end if;
         end if;
      end Scan_Back_End_Switches;

   --  Start of processing for Scan_Compiler_Arguments

   begin
      --  Acquire stack checking mode directly from GCC. The reason we do this
      --  is to make sure that the indication of stack checking being enabled
      --  is the same in the front end and the back end. This status obtained
      --  from gcc is affected by more than just the switch -fstack-check.

      Opt.Stack_Checking_Enabled := (flag_stack_check /= 0);

      --  Put the arguments in Args

      for Arg in Pos range 1 .. save_argc - 1 loop
         declare
            Argv_Ptr : constant Big_String_Ptr := save_argv (Arg);
            Argv_Len : constant Nat            := Len_Arg (Arg);
            Argv     : constant String         :=
                         Argv_Ptr (1 .. Natural (Argv_Len));
         begin
            Args (Positive (Arg)) := new String'(Argv);
         end;
      end loop;

      --  Loop through command line arguments, storing them for later access

      Next_Arg := 1;
      while Next_Arg <= Args'Last loop
         Look_At_Arg : declare
            Argv : constant String := Args (Next_Arg).all;

         begin
            --  If the previous switch has set the Output_File_Name_Present
            --  flag (that is we have seen a -gnatO), then the next argument
            --  is the name of the output object file.

            if Output_File_Name_Present and then not Output_File_Name_Seen then
               if Is_Switch (Argv) then
                  Fail ("Object file name missing after -gnatO");
               else
                  Set_Output_Object_File_Name (Argv);
                  Output_File_Name_Seen := True;
               end if;

            --  If the previous switch has set the Search_Directory_Present
            --  flag (that is if we have just seen -I), then the next argument
            --  is a search directory path.

            elsif Search_Directory_Present then
               if Is_Switch (Argv) then
                  Fail ("search directory missing after -I");
               else
                  Add_Src_Search_Dir (Argv);
                  Search_Directory_Present := False;
               end if;

            --  If not a switch, must be a file name

            elsif not Is_Switch (Argv) then
               Add_File (Argv);

            --  We must recognize -nostdinc to suppress visibility on the
            --  standard GNAT RTL sources. This is also a gcc switch.

            elsif Argv (Argv'First + 1 .. Argv'Last) = "nostdinc" then
               Opt.No_Stdinc := True;
               Scan_Back_End_Switches (Argv);

            --  We must recognize -nostdlib to suppress visibility on the
            --  standard GNAT RTL objects.

            elsif Argv (Argv'First + 1 .. Argv'Last) = "nostdlib" then
               Opt.No_Stdlib := True;

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

end Back_End;
