------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              G N A T M E M                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.3 $
--                                                                          --
--           Copyright (C) 1997-2001, Ada Core Technologies, Inc.           --
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

--  GNATMEM is a utility that tracks memory leaks. It is based on a simple
--  idea:
--      - run the application under gdb
--      - set a breakpoint on __gnat_malloc and __gnat_free
--      - record a reference to the allocated memory on each allocation call
--      - suppress this reference on deallocation
--      - at the end of the program, remaining references are potential leaks.
--        sort them out the best possible way in order to locate the root of
--        the leak.
--
--   GNATMEM can also be used with instrumented allocation/deallocation
--   routine (see a-raise.c with symbol GMEM defined). This is not supported
--   in all platforms, again refer to a-raise.c for further information.
--   In this case the application must be relinked with library libgmem.a:
--
--      $ gnatmake my_prog -largs -lgmem
--
--   The running my_prog will produce a file named gmem.out that will be
--   parsed by gnatmem.
--
--   In order to help finding out the real leaks,  the notion of "allocation
--   root" is defined. An allocation root is a specific point in the program
--   execution generating memory allocation where data is collected (such as
--   number of allocations, quantify of memory allocated, high water mark,
--   etc.).

with Ada.Command_Line;        use Ada.Command_Line;
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Text_IO.C_Streams;
with Ada.Float_Text_IO;
with Ada.Integer_Text_IO;
with Gnatvsn;                 use Gnatvsn;
with GNAT.Heap_Sort_G;
with GNAT.OS_Lib;
with GNAT.HTable;             use GNAT.HTable;
with Interfaces.C_Streams;    use Interfaces.C_Streams;
with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;

with Memroot; use Memroot;

procedure Gnatmem is

   ------------------------------------------------
   --  Potentially Target Dependent Subprograms. --
   ------------------------------------------------

   function Get_Current_TTY return String;
   --  Give the current tty on which the program is run. This is needed to
   --  separate the output of the debugger from the output of the program.
   --  The output of this function will be used to call the gdb command "tty"
   --  in the gdb script in order to get the program output on the current tty
   --  while the gdb output is redirected and processed by gnatmem.

   function popen  (File, Mode : System.Address) return FILEs;
   pragma Import (C, popen, "popen");
   --  Execute the program 'File'. If the mode is "r" the standard output
   --  of the program is redirected and the FILEs handler of the
   --  redirection is returned.

   procedure System_Cmd (X : System.Address);
   pragma Import (C, System_Cmd, "system");
   --  Execute the program "X".

   subtype Cstring        is String (1 .. Integer'Last);
   type    Cstring_Ptr is access all Cstring;

   function ttyname (Dec : Integer) return Cstring_Ptr;
   pragma Import (C, ttyname, "__gnat_ttyname");
   --  Return a null-terminated string containing the current tty

   Dir_Sep : constant Character := '/';

   ------------------------
   -- Other Declarations --
   ------------------------

   type Gdb_Output_Elmt is (Eof, Alloc, Deall);
   --  Eof    = End of gdb output file
   --  Alloc  = found a ALLOC mark in the gdb output
   --  Deall  = found a DEALL mark in the gdb output
   Gdb_Output_Format_Error : exception;

   function Read_Next return Gdb_Output_Elmt;
   --  Read the output of the debugger till it finds either the end of the
   --  output, or the 'ALLOC' mark or the 'DEALL' mark. In the second case,
   --  it sets the Tmp_Size and Tmp_Address global variables, in the
   --  third case it sets the Tmp_Address variable.

   procedure Create_Gdb_Script;
   --  Create the GDB script and save it in a temporary file

   function Mem_Image (X : Storage_Count) return String;
   --  X is a size in storage_element. Returns a value
   --  in Megabytes, Kiloytes or Bytes as appropriate.

   procedure Process_Arguments;
   --  Read command line arguments;

   procedure Usage;
   --  Prints out the option help

   function Gmem_Initialize (Dumpname : String) return Boolean;
   --  Opens the file represented by Dumpname and prepares it for
   --  work. Returns False if the file does not have the correct format, True
   --  otherwise.

   procedure Gmem_A2l_Initialize (Exename : String);
   --  Initialises the convert_addresses interface by supplying it with
   --  the name of the executable file Exename

   procedure Gmem_Read_Next (Buf : out String; Last : out Natural);
   --  Reads the next allocation/deallocation entry and its backtrace
   --  and prepares in the string Buf (up to the position of Last) the
   --  expression compatible with gnatmem parser:
   --  Allocation entry produces the expression "ALLOC^[size]^0x[address]^"
   --  Deallocation entry produces the expression "DEALLOC^0x[address]^"

   Argc        : constant Integer   := Argument_Count;
   Gnatmem_Tmp : aliased constant String    := "gnatmem.tmp";

   Mode_R : aliased constant String (1 .. 2) := 'r'  & ASCII.NUL;
   Mode_W : aliased constant String (1 .. 3) := "w+" & ASCII.NUL;

   -----------------------------------
   -- HTable address --> Allocation --
   -----------------------------------

   type Allocation is record
      Root : Root_Id;
      Size : Storage_Count;
   end record;

   type Address_Range is range 0 .. 4097;
   function H (A : Integer_Address) return Address_Range;
   No_Alloc : constant Allocation := (No_Root_Id, 0);

   package Address_HTable is new GNAT.HTable.Simple_HTable (
     Header_Num => Address_Range,
     Element    => Allocation,
     No_Element => No_Alloc,
     Key        => Integer_Address,
     Hash       => H,
     Equal      => "=");

   BT_Depth   : Integer := 1;
   FD         : FILEs;
   FT         : File_Type;
   File_Pos   : Integer := 0;
   Exec_Pos   : Integer := 0;
   Target_Pos : Integer := 0;
   Run_Gdb    : Boolean := True;

   Global_Alloc_Size      : Storage_Count  := 0;
   Global_High_Water_Mark : Storage_Count  := 0;
   Global_Nb_Alloc        : Integer        := 0;
   Global_Nb_Dealloc      : Integer        := 0;
   Nb_Root                : Integer        := 0;
   Nb_Wrong_Deall         : Integer        := 0;
   Target_Name            : String (1 .. 80);
   Target_Protocol        : String (1 .. 80);
   Target_Name_Len        : Integer;
   Target_Protocol_Len    : Integer;
   Cross_Case             : Boolean := False;

   Tmp_Size    : Storage_Count  := 0;
   Tmp_Address : Integer_Address;
   Tmp_Alloc   : Allocation;
   Quiet_Mode  : Boolean := False;

   --------------------------------
   -- GMEM functionality binding --
   --------------------------------

   function Gmem_Initialize (Dumpname : String) return Boolean is
      function Initialize (Dumpname : System.Address) return Boolean;
      pragma Import (C, Initialize, "__gnat_gmem_initialize");
      S : aliased String := Dumpname & ASCII.NUL;
   begin
      return Initialize (S'Address);
   end Gmem_Initialize;

   procedure Gmem_A2l_Initialize (Exename : String) is
      procedure A2l_Initialize (Exename : System.Address);
      pragma Import (C, A2l_Initialize, "__gnat_gmem_a2l_initialize");
      S : aliased String := Exename & ASCII.NUL;
   begin
      A2l_Initialize (S'Address);
   end Gmem_A2l_Initialize;

   procedure Gmem_Read_Next (Buf : out String; Last : out Natural) is
      procedure Read_Next (buf : System.Address);
      pragma Import (C, Read_Next, "__gnat_gmem_read_next");
      function Strlen (str : System.Address) return Natural;
      pragma Import (C, Strlen, "strlen");

      S : String (1 .. 1000);
   begin
      Read_Next (S'Address);
      Last := Strlen (S'Address);
      Buf (1 .. Last) := S (1 .. Last);
   end Gmem_Read_Next;

   ---------------------
   -- Get_Current_TTY --
   ---------------------

   function Get_Current_TTY return String is
      Res          :  Cstring_Ptr;
      stdout       : constant Integer := 1;
      Max_TTY_Name : constant Integer := 500;

   begin
      if isatty (stdout) /= 1 then
         return "";
      end if;

      Res := ttyname (1);
      if Res /= null then
         for J in Cstring'First .. Max_TTY_Name loop
            if Res (J) = ASCII.NUL then
               return Res (Cstring'First .. J - 1);
            end if;
         end loop;
      end if;

      --  if we fall thru the ttyname result was dubious. Just forget it.

      return "";
   end Get_Current_TTY;

   -------
   -- H --
   -------

   function H (A : Integer_Address) return Address_Range is
   begin
      return Address_Range (A mod Integer_Address (Address_Range'Last));
   end H;

   -----------------------
   -- Create_Gdb_Script --
   -----------------------

   procedure Create_Gdb_Script is
      FD : File_Type;

   begin
      begin
         Create (FD, Out_File, Gnatmem_Tmp);
      exception
         when others =>
            Put_Line ("Cannot create temporary file : " & Gnatmem_Tmp);
            GNAT.OS_Lib.OS_Exit (1);
      end;

      declare
         TTY : constant String := Get_Current_TTY;
      begin
         if TTY'Length > 0 then
            Put_Line (FD, "tty " & TTY);
         end if;
      end;

      if Cross_Case then
         Put (FD, "target ");
         Put (FD, Target_Protocol (1 .. Target_Protocol_Len));
         Put (FD, " ");
         Put (FD, Argument (Target_Pos));
         New_Line (FD);
         Put (FD, "load ");
         Put_Line (FD, Argument (Exec_Pos));

      else
         --  In the native case, run the program before setting the
         --  breakpoints so that gnatmem will also work with shared
         --  libraries.

         Put_Line (FD, "set lang c");
         Put_Line (FD, "break main");
         Put_Line (FD, "set lang auto");
         Put      (FD, "run");
         for J in Exec_Pos + 1 .. Argc loop
            Put (FD, " ");
            Put (FD, Argument (J));
         end loop;
         New_Line (FD);

         --  At this point, gdb knows about __gnat_malloc and __gnat_free
      end if;

      --  Make sure that outputing long backtraces do not pause

      Put_Line (FD, "set height 0");
      Put_Line (FD, "set width 0");

      if Quiet_Mode then
         Put_Line (FD, "break __gnat_malloc");
         Put_Line (FD, "command");
         Put_Line (FD, "   silent");
         Put_Line (FD, "   set lang c");
         Put_Line (FD, "   set print address on");
         Put_Line (FD, "   finish");
         Put_Line (FD, "   set $gm_addr = $");
         Put_Line (FD, "   printf ""\n\n""");
         Put_Line (FD, "   printf ""ALLOC^0x%x^\n"", $gm_addr");
         Put_Line (FD, "   set print address off");
         Put_Line (FD, "   set lang auto");
      else
         Put_Line (FD, "break __gnat_malloc");
         Put_Line (FD, "command");
         Put_Line (FD, "   silent");
         Put_Line (FD, "   set lang c");
         Put_Line (FD, "   set $gm_size = size");
         Put_Line (FD, "   set print address on");
         Put_Line (FD, "   finish");
         Put_Line (FD, "   set $gm_addr = $");
         Put_Line (FD, "   printf ""\n\n""");
         Put_Line (FD, "   printf ""ALLOC^%d^0x%x^\n"", $gm_size, $gm_addr");
         Put_Line (FD, "   set print address off");
         Put_Line (FD, "   set lang auto");
      end if;

      Put (FD, "   backtrace");

      if BT_Depth /= 0 then
         Put (FD, Integer'Image (BT_Depth));
      end if;

      New_Line (FD);

      Put_Line (FD, "   printf ""\n\n""");
      Put_Line (FD, "   continue");
      Put_Line (FD, "end");
      Put_Line (FD, "#");
      Put_Line (FD, "#");
      Put_Line (FD, "break __gnat_free");
      Put_Line (FD, "command");
      Put_Line (FD, "   silent");
      Put_Line (FD, "   set print address on");
      Put_Line (FD, "   printf ""\n\n""");
      Put_Line (FD, "   printf ""DEALL^0x%x^\n"", ptr");
      Put_Line (FD, "   set print address off");
      Put_Line (FD, "   finish");

      Put (FD, "   backtrace");

      if BT_Depth /= 0 then
         Put (FD, Integer'Image (BT_Depth));
      end if;

      New_Line (FD);

      Put_Line (FD, "   printf ""\n\n""");
      Put_Line (FD, "   continue");
      Put_Line (FD, "end");
      Put_Line (FD, "#");
      Put_Line (FD, "#");
      Put_Line (FD, "#");

      if Cross_Case then
         Put (FD, "run ");
         Put_Line (FD, Argument (Exec_Pos));

         if Target_Protocol (1 .. Target_Protocol_Len) = "wtx" then
            Put (FD, "unload ");
            Put_Line (FD, Argument (Exec_Pos));
         end if;
      else
         Put_Line (FD, "continue");
      end if;

      Close (FD);
   end Create_Gdb_Script;

   ---------------
   -- Mem_Image --
   ---------------

   function Mem_Image (X : Storage_Count) return String is
      Ks    : constant Storage_Count := X / 1024;
      Megs  : constant Storage_Count := Ks / 1024;
      Buff  : String (1 .. 7);

   begin
      if Megs /= 0 then
         Ada.Float_Text_IO.Put (Buff, Float (X) / 1024.0 / 1024.0, 2, 0);
         return Buff & " Megabytes";

      elsif Ks /= 0 then
         Ada.Float_Text_IO.Put (Buff, Float (X) / 1024.0, 2, 0);
         return Buff & " Kilobytes";

      else
         Ada.Integer_Text_IO.Put (Buff (1 .. 4), Integer (X));
         return  Buff (1 .. 4) & " Bytes";
      end if;
   end Mem_Image;

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      New_Line;
      Put ("GNATMEM ");
      Put (Gnat_Version_String);
      Put_Line (" Copyright 1997-2000 Free Software Foundation, Inc.");
      New_Line;

      if Cross_Case then
         Put_Line (Command_Name
           & " [-q] [n] [-o file] target entry_point ...");
         Put_Line (Command_Name & " [-q] [n] [-i file]");

      else
         Put_Line ("GDB mode");
         Put_Line ("   " & Command_Name
                   & " [-q] [n] [-o file] program arg1 arg2 ...");
         Put_Line ("   " & Command_Name
                   & " [-q] [n] [-i file]");
         New_Line;
         Put_Line ("GMEM mode");
         Put_Line ("   " & Command_Name
                   & " [-q] [n] -i gmem.out program arg1 arg2 ...");
         New_Line;
      end if;

      Put_Line ("  -q       quiet, minimum output");
      Put_Line ("   n       number of frames for allocation root backtraces");
      Put_Line ("           default is 1.");
      Put_Line ("  -o file  save gdb output in 'file' and process data");
      Put_Line ("           post mortem. also keep the gdb script around");
      Put_Line ("  -i file  don't run gdb output. Do only post mortem");
      Put_Line ("           processing from file");
      GNAT.OS_Lib.OS_Exit (1);
   end Usage;

   -----------------------
   -- Process_Arguments --
   -----------------------

   procedure Process_Arguments is
      Arg : Integer;

      procedure Check_File (Arg_Pos : Integer; For_Creat : Boolean := False);
      --  Check that Argument (Arg_Pos) is an existing file if For_Creat is
      --  false or if it is possible to create it if For_Creat is true

      procedure Check_File (Arg_Pos : Integer; For_Creat : Boolean := False) is
         Name : aliased constant String := Argument (Arg_Pos) & ASCII.NUL;
         X    : int;

      begin
         if For_Creat then
            FD := fopen (Name'Address, Mode_W'Address);
         else
            FD := fopen (Name'Address, Mode_R'Address);
         end if;

         if FD = NULL_Stream then
            New_Line;
            if For_Creat then
               Put_Line ("Cannot create file : " & Argument (Arg_Pos));
            else
               Put_Line ("Cannot locate file : " & Argument (Arg_Pos));
            end if;
            New_Line;
            Usage;
         else
            X := fclose (FD);
         end if;
      end Check_File;

   --  Start of processing for Process_Arguments

   begin

      --  Is it a cross version?

      declare
         Std_Name : constant String  := "gnatmem";
         Name     : constant String  := Command_Name;
         End_Pref : constant Integer := Name'Last - Std_Name'Length;

      begin
         if Name'Length > Std_Name'Length + 9
           and then
             Name (End_Pref + 1 .. Name'Last) = Std_Name
           and then
             Name (End_Pref - 8 .. End_Pref) = "-vxworks-"
         then
            Cross_Case := True;

            Target_Name_Len := End_Pref - 1;
            for J in reverse Name'First .. End_Pref - 1 loop
               if Name (J) = Dir_Sep then
                  Target_Name_Len := Target_Name_Len - J;
                  exit;
               end if;
            end loop;

            Target_Name (1 .. Target_Name_Len)
              := Name (End_Pref - Target_Name_Len  .. End_Pref - 1);

            if Target_Name (1 .. 5) = "alpha" then
               Target_Protocol (1 .. 7) := "vxworks";
               Target_Protocol_Len := 7;
            else
               Target_Protocol (1 .. 3) := "wtx";
               Target_Protocol_Len := 3;
            end if;
         end if;
      end;

      Arg := 1;

      if Argc < Arg then
         Usage;
      end if;

      --  Deal with "-q"

      if Argument (Arg) = "-q" then

         Quiet_Mode := True;
         Arg := Arg + 1;

         if Argc < Arg then
            Usage;
         end if;
      end if;

      --  Deal with back trace depth

      if Argument (Arg) (1) in '0' .. '9' then
         begin
            BT_Depth := Integer'Value (Argument (Arg));
         exception
            when others =>
               Usage;
         end;

         Arg := Arg + 1;

         if Argc < Arg then
            Usage;
         end if;
      end if;

      --  Deal with "-o file" or "-i file"

      while Arg <= Argc and then Argument (Arg) (1) = '-' loop
         Arg := Arg + 1;

         if Argc < Arg then
            Usage;
         end if;

         case Argument (Arg - 1) (2) is
            when 'o' =>
               Check_File (Arg, For_Creat => True);
               File_Pos := Arg;

            when 'i' =>
               Check_File (Arg);
               File_Pos := Arg;
               Run_Gdb  := False;
               if Gmem_Initialize (Argument (Arg)) then
                  Gmem_Mode := True;
               end if;

            when others =>
               Put_Line ("Unknown option : " & Argument (Arg));
               Usage;
         end case;

         Arg := Arg + 1;

         if Argc < Arg and then Run_Gdb then
            Usage;
         end if;
      end loop;

      --  In the cross case, we first get the target

      if Cross_Case then
         Target_Pos := Arg;
         Arg := Arg + 1;

         if Argc < Arg and then Run_Gdb then
            Usage;
         end if;
      end if;

      --  Now all the following arguments are to be passed to gdb

      if Run_Gdb then
         Exec_Pos := Arg;
         Check_File (Exec_Pos);

      elsif Gmem_Mode then
         if Arg > Argc then
            Usage;
         else
            Exec_Pos := Arg;
            Check_File (Exec_Pos);
            Gmem_A2l_Initialize (Argument (Exec_Pos));
         end if;

      --  ... in other cases further arguments are disallowed

      elsif Arg <= Argc then
         Usage;
      end if;
   end Process_Arguments;

   ---------------
   -- Read_Next --
   ---------------

   function Read_Next return Gdb_Output_Elmt is
      Max_Line : constant Integer   := 100;
      Line     : String (1 .. Max_Line);
      Last     : Integer := 0;

      Curs1, Curs2 : Integer;
      Separator    : constant Character := '^';

      function Next_Separator return Integer;
      --  Return the index of the next separator after Curs1 in Line

      function Next_Separator return Integer is
         Curs : Integer := Curs1;

      begin
         loop
            if Curs > Last then
               raise Gdb_Output_Format_Error;

            elsif Line (Curs) = Separator then
               return Curs;
            end if;

            Curs := Curs + 1;
         end loop;
      end Next_Separator;

   --  Start of processing for Read_Next

   begin
      Line (1) := ' ';

      loop
         if Gmem_Mode then
            Gmem_Read_Next (Line, Last);
         else
            Get_Line (FT, Line, Last);
         end if;

         if Line (1 .. 14) = "Program exited" then
            return Eof;

         elsif Line (1 .. 5) = "ALLOC" then
            --  ALLOC ^ <size> ^0x <addr> ^

            --  Read the size

            Curs1 := 7;
            Curs2 := Next_Separator - 1;

            if not Quiet_Mode then
               Tmp_Size := Storage_Count'Value (Line (Curs1 .. Curs2));
            end if;

            --  Read the address, skip "^0x"

            Curs1 := Curs2 + 4;
            Curs2 := Next_Separator - 1;
            Tmp_Address := Integer_Address'Value (
                               "16#" & Line (Curs1 .. Curs2) & "#");
            return Alloc;

         elsif Line (1 .. 5) = "DEALL" then
            --  DEALL ^ 0x <addr> ^

            --  Read the address, skip "^0x"

            Curs1 := 9;
            Curs2 := Next_Separator - 1;
            Tmp_Address := Integer_Address'Value (
                               "16#" & Line (Curs1 .. Curs2) & "#");
            return Deall;
         end if;
      end loop;
   exception
      when End_Error =>
         New_Line;
         Put_Line ("### incorrect user program  termination detected.");
         Put_Line ("    following data may not be meaningful");
         New_Line;
         return Eof;
   end Read_Next;

--  Start of processing for Gnatmem

begin
   Process_Arguments;

   if Run_Gdb then
      Create_Gdb_Script;
   end if;

   --  Now we start the gdb session using the following syntax

   --     gdb --nx --nw -batch -x gnatmem.tmp

   --  If there is a -o option we redirect the gdb output in the specified
   --  file, otherwise we just read directly from a pipe.

   if File_Pos /= 0 then
      declare
         Name : aliased String := Argument (File_Pos) & ASCII.NUL;

      begin
         if Run_Gdb then
            if Cross_Case then
               declare
                  Cmd : aliased String := Target_Name (1 .. Target_Name_Len)
                    & "-gdb --nx --nw -batch -x " & Gnatmem_Tmp & " > "
                    & Name;
               begin
                  System_Cmd (Cmd'Address);
               end;
            else

               declare
                  Cmd : aliased String
                    := "gdb --nx --nw " & Argument (Exec_Pos)
                           & " -batch -x " & Gnatmem_Tmp & " > "
                           & Name;
               begin
                  System_Cmd (Cmd'Address);
               end;
            end if;
         end if;

         if not Gmem_Mode then
            FD := fopen (Name'Address, Mode_R'Address);
         end if;
      end;

   else
      if Cross_Case then
         declare
            Cmd : aliased String := Target_Name (1 .. Target_Name_Len)
              & "-gdb --nx --nw -batch -x " & Gnatmem_Tmp & ASCII.NUL;
         begin
            FD := popen (Cmd'Address, Mode_R'Address);
         end;
      else
         declare
            Cmd : aliased String := "gdb --nx --nw " & Argument (Exec_Pos)
              & " -batch -x " & Gnatmem_Tmp & ASCII.NUL;

         begin
            FD := popen (Cmd'Address, Mode_R'Address);
         end;
      end if;
   end if;

   --  Open the FD file as a regular Text_IO file

   if not Gmem_Mode then
      Ada.Text_IO.C_Streams.Open (FT, In_File, FD);
   end if;

   --  Main loop  analysing the data generated by the debugger
   --  for each allocation, the backtrace is kept and stored in a htable
   --  whose entry is the address. Fore ach deallocation, we look for the
   --  corresponding allocation and cancel it.

   Main : loop
      case Read_Next is
         when EOF =>
            exit Main;

         when Alloc =>

            --  Update global counters if the allocated size is meaningful

            if Quiet_Mode then
               Tmp_Alloc.Root := Read_BT (BT_Depth, FT);
               if Nb_Alloc (Tmp_Alloc.Root) = 0 then
                  Nb_Root := Nb_Root + 1;
               end if;
               Set_Nb_Alloc (Tmp_Alloc.Root, Nb_Alloc (Tmp_Alloc.Root) + 1);
               Address_HTable.Set (Tmp_Address, Tmp_Alloc);

            elsif Tmp_Size > 0 then

               Global_Alloc_Size := Global_Alloc_Size + Tmp_Size;
               Global_Nb_Alloc   := Global_Nb_Alloc + 1;

               if Global_High_Water_Mark < Global_Alloc_Size then
                  Global_High_Water_Mark := Global_Alloc_Size;
               end if;

               --  Read the corresponding back trace

               Tmp_Alloc.Root := Read_BT (BT_Depth, FT);

               --  Update the number of allocation root if this is a new one

               if Nb_Alloc (Tmp_Alloc.Root) = 0 then
                  Nb_Root := Nb_Root + 1;
               end if;

               --  Update allocation root specific counters

               Set_Alloc_Size (Tmp_Alloc.Root,
                 Alloc_Size (Tmp_Alloc.Root) + Tmp_Size);

               Set_Nb_Alloc (Tmp_Alloc.Root, Nb_Alloc (Tmp_Alloc.Root) + 1);

               if High_Water_Mark (Tmp_Alloc.Root)
                  < Alloc_Size (Tmp_Alloc.Root)
               then
                  Set_High_Water_Mark (Tmp_Alloc.Root,
                    Alloc_Size (Tmp_Alloc.Root));
               end if;

               --  Associate this allocation root to the allocated address

               Tmp_Alloc.Size := Tmp_Size;
               Address_HTable.Set (Tmp_Address, Tmp_Alloc);

            --  non meaninful output, just consumes the backtrace

            else
               Tmp_Alloc.Root := Read_BT (BT_Depth, FT);
            end if;

         when Deall =>

            --  Get the corresponding Dealloc_Size and Root

            Tmp_Alloc := Address_HTable.Get (Tmp_Address);

            if Tmp_Alloc.Root = No_Root_Id then

               --  There was no prior allocation at this address, something is
               --  very wrong. Mark this allocation root as problematic a

               Tmp_Alloc.Root := Read_BT (BT_Depth, FT);

               if Nb_Alloc (Tmp_Alloc.Root) = 0 then
                  Set_Nb_Alloc (Tmp_Alloc.Root, Nb_Alloc (Tmp_Alloc.Root) - 1);
                  Nb_Wrong_Deall := Nb_Wrong_Deall + 1;
               end if;

            else
               --  Update global counters

               if not Quiet_Mode then
                  Global_Alloc_Size := Global_Alloc_Size - Tmp_Alloc.Size;
               end if;
               Global_Nb_Dealloc   := Global_Nb_Dealloc + 1;

               --  Update allocation root specific counters

               if not Quiet_Mode then
                  Set_Alloc_Size (Tmp_Alloc.Root,
                    Alloc_Size (Tmp_Alloc.Root) - Tmp_Alloc.Size);
               end if;
               Set_Nb_Alloc (Tmp_Alloc.Root, Nb_Alloc (Tmp_Alloc.Root) - 1);

               --  update the number of allocation root if this one disappear

               if Nb_Alloc (Tmp_Alloc.Root) = 0 then
                  Nb_Root := Nb_Root - 1;
               end if;

               --  De-associate the deallocated address

               Address_HTable.Remove (Tmp_Address);
            end if;
      end case;
   end loop Main;

   --  We can get rid of the temp file now

   if Run_Gdb and then File_Pos = 0 then
      declare
         X : int;
      begin
         X := unlink (Gnatmem_Tmp'Address);
      end;
   end if;

   --  Print out general information about overall allocation

   if not Quiet_Mode then
      Put_Line ("Global information");
      Put_Line ("------------------");

      Put      ("   Total number of allocations        :");
      Ada.Integer_Text_IO.Put (Global_Nb_Alloc, 4);
      New_Line;

      Put      ("   Total number of deallocations      :");
      Ada.Integer_Text_IO.Put (Global_Nb_Dealloc, 4);
      New_Line;

      Put_Line ("   Final Water Mark (non freed mem)   :"
        & Mem_Image (Global_Alloc_Size));
      Put_Line ("   High Water Mark                    :"
        & Mem_Image (Global_High_Water_Mark));
      New_Line;
   end if;

   --  Print out the back traces corresponding to potential leaks in order
   --  greatest number of non-deallocated allocations

   Print_Back_Traces : declare
      type Root_Array is array (Natural range <>) of Root_Id;
      Leaks   : Root_Array (0 .. Nb_Root);
      Leak_Index   : Natural := 0;

      Bogus_Dealls : Root_Array (1 .. Nb_Wrong_Deall);
      Deall_Index  : Natural := 0;

      procedure Move (From : Natural; To : Natural);
      function  Lt (Op1, Op2 : Natural) return Boolean;
      package   Root_Sort is new GNAT.Heap_Sort_G (Move, Lt);

      procedure Move (From : Natural; To : Natural) is
      begin
         Leaks (To) := Leaks (From);
      end Move;

      function Lt (Op1, Op2 : Natural) return Boolean is
      begin
         if Nb_Alloc (Leaks (Op1)) > Nb_Alloc (Leaks (Op2)) then
            return True;
         elsif  Nb_Alloc (Leaks (Op1)) = Nb_Alloc (Leaks (Op2)) then
            return Alloc_Size (Leaks (Op1)) > Alloc_Size (Leaks (Op2));
         else
            return False;
         end if;
      end Lt;

   --  Start of processing for Print_Back_Traces

   begin
      --  Transfer all the relevant Roots in the Leaks and a
      --  Bogus_Deall arrays

      Tmp_Alloc.Root := Get_First;
      while Tmp_Alloc.Root /= No_Root_Id loop
         if Nb_Alloc (Tmp_Alloc.Root) = 0 then
            null;

         elsif Nb_Alloc (Tmp_Alloc.Root) < 0  then
            Deall_Index := Deall_Index + 1;
            Bogus_Dealls (Deall_Index) := Tmp_Alloc.Root;

         else
            Leak_Index := Leak_Index + 1;
            Leaks (Leak_Index) := Tmp_Alloc.Root;
         end if;

         Tmp_Alloc.Root := Get_Next;
      end loop;

      --  Print out wrong deallocations

      if Nb_Wrong_Deall > 0 then
         Put_Line    ("Releasing deallocated memory at :");
         if not Quiet_Mode then
            Put_Line ("--------------------------------");
         end if;

         for J in  1 .. Bogus_Dealls'Last loop
            Print_BT (Bogus_Dealls (J));
            New_Line;
         end loop;
      end if;

      --  Print out all allocation Leaks

      if Nb_Root > 0 then

         --  Sort the Leaks so that potentially important leaks appear first

         Root_Sort.Sort (Nb_Root);

         for J in  1 .. Leaks'Last loop
            if Quiet_Mode then
               if Nb_Alloc (Leaks (J)) = 1 then
                  Put_Line (Integer'Image (Nb_Alloc (Leaks (J)))
                    & " leak at :");
               else
                  Put_Line (Integer'Image (Nb_Alloc (Leaks (J)))
                    & " leaks at :");
               end if;
            else
               Put_Line ("Allocation Root #" & Integer'Image (J));
               Put_Line ("-------------------");

               Put      (" Number of non freed allocations    :");
               Ada.Integer_Text_IO.Put (Nb_Alloc (Leaks (J)), 4);
               New_Line;

               Put_Line (" Final Water Mark (non freed mem)   :"
                 & Mem_Image (Alloc_Size (Leaks (J))));

               Put_Line (" High Water Mark                    :"
                 & Mem_Image (High_Water_Mark (Leaks (J))));

               Put_Line (" Backtrace                          :");
            end if;
            Print_BT (Leaks (J));
            New_Line;
         end loop;
      end if;
   end Print_Back_Traces;

end Gnatmem;
