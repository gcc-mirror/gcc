------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                            B A C K _ E N D                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2006, Free Software Foundation, Inc.         --
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

with Atree;     use Atree;
with Debug;     use Debug;
with Elists;    use Elists;
with Lib;       use Lib;
with Osint;     use Osint;
with Opt;       use Opt;
with Osint.C;   use Osint.C;
with Namet;     use Namet;
with Nlists;    use Nlists;
with Stand;     use Stand;
with Sinput;    use Sinput;
with Stringt;   use Stringt;
with Switch;    use Switch;
with Switch.C;  use Switch.C;
with System;    use System;
with Types;     use Types;

package body Back_End is

   -------------------
   -- Call_Back_End --
   -------------------

   procedure Call_Back_End (Mode : Back_End_Mode_Type) is

      --  The File_Record type has a lot of components that are meaningless
      --  to the back end, so a new record is created here to contain the
      --  needed information for each file.

      type Needed_File_Info_Type is record
         File_Name        : File_Name_Type;
         First_Sloc       : Source_Ptr;
         Last_Sloc        : Source_Ptr;
         Num_Source_Lines : Nat;
      end record;

      File_Info_Array :
        array (Main_Unit .. Last_Unit) of Needed_File_Info_Type;

      procedure gigi (
         gnat_root                     : Int;
         max_gnat_node                 : Int;
         number_name                   : Nat;
         nodes_ptr                     : Address;

         next_node_ptr                 : Address;
         prev_node_ptr                 : Address;
         elists_ptr                    : Address;
         elmts_ptr                     : Address;

         strings_ptr                   : Address;
         string_chars_ptr              : Address;
         list_headers_ptr              : Address;
         number_units                  : Int;

         file_info_ptr                 : Address;
         gigi_standard_integer         : Entity_Id;
         gigi_standard_long_long_float : Entity_Id;
         gigi_standard_exception_type  : Entity_Id;
         gigi_operating_mode           : Back_End_Mode_Type);

      pragma Import (C, gigi);

      S : Source_File_Index;

   begin
      --  Skip call if in -gnatdH mode

      if Debug_Flag_HH then
         return;
      end if;

      for J in Main_Unit .. Last_Unit loop
         S := Source_Index (J);
         File_Info_Array (J).File_Name        := File_Name (S);
         File_Info_Array (J).First_Sloc       := Source_Text (S)'First;
         File_Info_Array (J).Last_Sloc        := Source_Text (S)'Last;
         File_Info_Array (J).Num_Source_Lines := Num_Source_Lines (S);
      end loop;

      gigi (
         gnat_root          => Int (Cunit (Main_Unit)),
         max_gnat_node      => Int (Last_Node_Id - First_Node_Id + 1),
         number_name        => Name_Entries_Count,
         nodes_ptr          => Nodes_Address,

         next_node_ptr      => Next_Node_Address,
         prev_node_ptr      => Prev_Node_Address,
         elists_ptr         => Elists_Address,
         elmts_ptr          => Elmts_Address,

         strings_ptr        => Strings_Address,
         string_chars_ptr   => String_Chars_Address,
         list_headers_ptr   => Lists_Address,
         number_units       => Num_Units,

         file_info_ptr                 => File_Info_Array'Address,
         gigi_standard_integer         => Standard_Integer,
         gigi_standard_long_long_float => Standard_Long_Long_Float,
         gigi_standard_exception_type  => Standard_Exception_Type,
         gigi_operating_mode           => Mode);
   end Call_Back_End;

   -----------------------------
   -- Scan_Compiler_Arguments --
   -----------------------------

   procedure Scan_Compiler_Arguments is

      Next_Arg : Pos := 1;

      subtype Big_String is String (Positive);
      type BSP is access Big_String;

      type Arg_Array is array (Nat) of BSP;
      type Arg_Array_Ptr is access Arg_Array;

      --  Import flag_stack_check from toplev.c

      flag_stack_check : Int;
      pragma Import (C, flag_stack_check); -- Import from toplev.c

      save_argc : Nat;
      pragma Import (C, save_argc); -- Import from toplev.c

      save_argv : Arg_Array_Ptr;
      pragma Import (C, save_argv); -- Import from toplev.c

      Output_File_Name_Seen : Boolean := False;
      --  Set to True after having scanned the file_name for
      --  switch "-gnatO file_name"

      --  Local functions

      function Len_Arg (Arg : Pos) return Nat;
      --  Determine length of argument number Arg on the original
      --  command line from gnat1

      procedure Scan_Back_End_Switches (Switch_Chars : String);
      --  Procedure to scan out switches stored in Switch_Chars. The first
      --  character is known to be a valid switch character, and there are no
      --  blanks or other switch terminator characters in the string, so the
      --  entire string should consist of valid switch characters, except that
      --  an optional terminating NUL character is allowed.
      --
      --  Back end switches have already been checked and processed by GCC
      --  in toplev.c, so no errors can occur and control will always return.
      --  The switches must still be scanned to skip the arguments of the
      --  "-o" or the (undocumented) "-dumpbase" switch, by incrementing
      --  the Next_Arg variable. The "-dumpbase" switch is used to set the
      --  basename for GCC dumpfiles.

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

      ----------------------------
      -- Scan_Back_End_Switches --
      ----------------------------

      procedure Scan_Back_End_Switches (Switch_Chars : String) is
         First : constant Positive := Switch_Chars'First + 1;
         Last  : Natural := Switch_Chars'Last;

      begin
         if Last >= First
           and then Switch_Chars (Last) = ASCII.NUL
         then
            Last := Last - 1;
         end if;

         --  For these switches, skip following argument and do not
         --  store either the switch or the following argument

         if Switch_Chars (First .. Last) = "o"
            or else Switch_Chars (First .. Last) = "dumpbase"
            or else Switch_Chars (First .. Last) = "-param"

         then
            Next_Arg := Next_Arg + 1;

         --  Do not record -quiet switch

         elsif Switch_Chars (First .. Last) = "quiet" then
            null;

         else
            --  Store any other GCC switches

            Store_Compilation_Switch (Switch_Chars);
         end if;
      end Scan_Back_End_Switches;

   --  Start of processing for Scan_Compiler_Arguments

   begin
      --  Acquire stack checking mode directly from GCC

      Opt.Stack_Checking_Enabled := (flag_stack_check /= 0);

      --  Loop through command line arguments, storing them for later access

      while Next_Arg < save_argc loop
         Look_At_Arg : declare
            Argv_Ptr : constant BSP    := save_argv (Next_Arg);
            Argv_Len : constant Nat    := Len_Arg (Next_Arg);
            Argv     : constant String := Argv_Ptr (1 .. Natural (Argv_Len));

         begin
            --  If the previous switch has set the Output_File_Name_Present
            --  flag (that is we have seen a -gnatO), then the next argument
            --  is the name of the output object file.

            if Output_File_Name_Present
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
                  Search_Directory_Present := False;
               end if;

            elsif not Is_Switch (Argv) then -- must be a file name
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
               Scan_Front_End_Switches (Argv);

            --  All non-front-end switches are back-end switches

            else
               Scan_Back_End_Switches (Argv);
            end if;
         end Look_At_Arg;

         Next_Arg := Next_Arg + 1;
      end loop;
   end Scan_Compiler_Arguments;

end Back_End;
