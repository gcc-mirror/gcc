------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               G N A T L S                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.3 $
--                                                                          --
--           Copyright (C) 1992-2001 Free Software Foundation, Inc.         --
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

with ALI;         use ALI;
with ALI.Util;    use ALI.Util;
with Binderr;     use Binderr;
with Butil;       use Butil;
with Csets;
with Fname;       use Fname;
with Gnatvsn;     use Gnatvsn;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Namet;       use Namet;
with Opt;         use Opt;
with Osint;       use Osint;
with Output;      use Output;
with Prj;         use Prj;
with Prj.Pars;    use Prj.Pars;
with Prj.Env;
with Prj.Ext;     use Prj.Ext;
with Prj.Util;    use Prj.Util;
with Snames;      use Snames;
with Stringt;     use Stringt;
with Types;       use Types;

procedure Gnatls is
   pragma Ident (Gnat_Version_String);

   Max_Column : constant := 80;

   type File_Status is (
     OK,                  --  matching timestamp
     Checksum_OK,         --  only matching checksum
     Not_Found,           --  file not found on source PATH
     Not_Same,            --  neither checksum nor timestamp matching
     Not_First_On_PATH);  --  matching file hidden by Not_Same file on path

   type Dir_Data;
   type Dir_Ref is access Dir_Data;

   type Dir_Data is record
      Value : String_Access;
      Next  : Dir_Ref;
   end record;

   First_Source_Dir : Dir_Ref;
   Last_Source_Dir  : Dir_Ref;
   --  The list of source directories from the command line.
   --  These directories are added using Osint.Add_Src_Search_Dir
   --  after those of the GNAT Project File, if any.

   First_Lib_Dir : Dir_Ref;
   Last_Lib_Dir  : Dir_Ref;
   --  The list of object directories from the command line.
   --  These directories are added using Osint.Add_Lib_Search_Dir
   --  after those of the GNAT Project File, if any.

   Main_File : File_Name_Type;
   Ali_File  : File_Name_Type;

   Text : Text_Buffer_Ptr;
   Id   : ALI_Id;

   Next_Arg : Positive;

   Too_Long : Boolean := False;
   --  When True, lines are too long for multi-column output and each
   --  item of information is on a different line.

   Project_File      : String_Access;
   Project           : Prj.Project_Id;
   Current_Verbosity : Prj.Verbosity := Prj.Default;

   Selective_Output : Boolean := False;
   Print_Usage      : Boolean := False;
   Print_Unit       : Boolean := True;
   Print_Source     : Boolean := True;
   Print_Object     : Boolean := True;
   --  Flags controlling the form of the outpout

   Dependable       : Boolean := False;  --  flag -d
   Also_Predef      : Boolean := False;

   Unit_Start   : Integer;
   Unit_End     : Integer;
   Source_Start : Integer;
   Source_End   : Integer;
   Object_Start : Integer;
   Object_End   : Integer;
   --  Various column starts and ends

   Spaces : constant String (1 .. Max_Column) := (others => ' ');

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Add_Lib_Dir (Dir : String; And_Save : Boolean);
   --  Add an object directory, using Osint.Add_Lib_Search_Dir
   --  if And_Save is False or keeping in the list First_Lib_Dir,
   --  Last_Lib_Dir if And_Save is True.

   procedure Add_Source_Dir (Dir : String; And_Save : Boolean);
   --  Add a source directory, using Osint.Add_Src_Search_Dir
   --  if And_Save is False or keeping in the list First_Source_Dir,
   --  Last_Source_Dir if And_Save is True.

   procedure Find_General_Layout;
   --  Determine the structure of the output (multi columns or not, etc)

   procedure Find_Status
     (FS       : in out File_Name_Type;
      Stamp    : Time_Stamp_Type;
      Checksum : Word;
      Status   : out File_Status);
   --  Determine the file status (Status) of the file represented by FS
   --  with the expected Stamp and checksum given as argument. FS will be
   --  updated to the full file name if available.

   function Corresponding_Sdep_Entry (A : ALI_Id; U : Unit_Id) return Sdep_Id;
   --  Give the Sdep entry corresponding to the unit U in ali record A.

   function Index (Char : Character; Str : String) return Natural;
   --  Returns the first occurrence of Char in Str.
   --  Returns 0 if Char is not in Str.

   procedure Output_Object (O : File_Name_Type);
   --  Print out the name of the object when requested

   procedure Output_Source (Sdep_I : Sdep_Id);
   --  Print out the name and status of the source corresponding to this
   --  sdep entry

   procedure Output_Status (FS : File_Status; Verbose : Boolean);
   --  Print out FS either in a coded form if verbose is false or in an
   --  expanded form otherwise.

   procedure Output_Unit (U_Id : Unit_Id);
   --  Print out information on the unit when requested

   procedure Reset_Print;
   --  Reset Print flags properly when selective output is chosen

   procedure Scan_Ls_Arg (Argv : String; And_Save : Boolean);
   --  Scan and process lser specific arguments. Argv is a single argument.

   procedure Usage;
   --  Print usage message.

   -----------------
   -- Add_Lib_Dir --
   -----------------

   procedure Add_Lib_Dir (Dir : String; And_Save : Boolean) is
   begin
      if And_Save then
         if First_Lib_Dir = null then
            First_Lib_Dir :=
              new Dir_Data'
                (Value => new String'(Dir),
                 Next => null);
            Last_Lib_Dir := First_Lib_Dir;

         else
            Last_Lib_Dir.Next :=
              new Dir_Data'
                (Value => new String'(Dir),
                 Next => null);
            Last_Lib_Dir := Last_Lib_Dir.Next;
         end if;

      else
         Add_Lib_Search_Dir (Dir);
      end if;
   end Add_Lib_Dir;

   -- -----------------
   -- Add_Source_Dir --
   --------------------

   procedure Add_Source_Dir (Dir : String; And_Save : Boolean) is
   begin
      if And_Save then
         if First_Source_Dir = null then
            First_Source_Dir :=
              new Dir_Data'
                (Value => new String'(Dir),
                 Next => null);
            Last_Source_Dir := First_Source_Dir;

         else
            Last_Source_Dir.Next :=
              new Dir_Data'
                (Value => new String'(Dir),
                 Next => null);
            Last_Source_Dir := Last_Source_Dir.Next;
         end if;

      else
         Add_Src_Search_Dir (Dir);
      end if;
   end Add_Source_Dir;

   ------------------------------
   -- Corresponding_Sdep_Entry --
   ------------------------------

   function Corresponding_Sdep_Entry
     (A     : ALI_Id;
      U     : Unit_Id)
      return  Sdep_Id
   is
   begin
      for D in ALIs.Table (A).First_Sdep .. ALIs.Table (A).Last_Sdep loop
         if Sdep.Table (D).Sfile = Units.Table (U).Sfile then
            return D;
         end if;
      end loop;

      Error_Msg_Name_1 := Units.Table (U).Uname;
      Error_Msg_Name_2 := ALIs.Table (A).Afile;
      Write_Eol;
      Error_Msg ("wrong ALI format, can't find dependency line for & in %");
      Exit_Program (E_Fatal);

      --  Not needed since we exit the program but avoids compiler warning

      raise Program_Error;
   end Corresponding_Sdep_Entry;

   -------------------------
   -- Find_General_Layout --
   -------------------------

   procedure Find_General_Layout is
      Max_Unit_Length : Integer := 11;
      Max_Src_Length  : Integer := 11;
      Max_Obj_Length  : Integer := 11;

      Len : Integer;
      FS  : File_Name_Type;

   begin
      --  Compute maximum of each column

      for Id in ALIs.First .. ALIs.Last loop

         Get_Name_String (Units.Table (ALIs.Table (Id).First_Unit).Uname);
         if Also_Predef or else not Is_Internal_Unit then

            if Print_Unit then
               Len := Name_Len - 1;
               Max_Unit_Length := Integer'Max (Max_Unit_Length, Len);
            end if;

            if Print_Source then
               FS := Full_Source_Name (ALIs.Table (Id).Sfile);

               if FS = No_File then
                  Get_Name_String (ALIs.Table (Id).Sfile);
                  Name_Len := Name_Len + 13;
               else
                  Get_Name_String (FS);
               end if;

               Max_Src_Length := Integer'Max (Max_Src_Length, Name_Len + 1);
            end if;

            if Print_Object then
               Get_Name_String (ALIs.Table (Id).Ofile_Full_Name);
               Max_Obj_Length := Integer'Max (Max_Obj_Length, Name_Len + 1);
            end if;
         end if;
      end loop;

      --  Verify is output is not wider than maximum number of columns

      Too_Long := Verbose_Mode or else
        (Max_Unit_Length + Max_Src_Length + Max_Obj_Length) > Max_Column;

      --  Set start and end of columns.

      Object_Start := 1;
      Object_End   := Object_Start - 1;

      if Print_Object then
         Object_End   := Object_Start + Max_Obj_Length;
      end if;

      Unit_Start := Object_End + 1;
      Unit_End   := Unit_Start - 1;

      if Print_Unit then
         Unit_End   := Unit_Start + Max_Unit_Length;
      end if;

      Source_Start := Unit_End + 1;
      if Source_Start > Spaces'Last then
         Source_Start := Spaces'Last;
      end if;
      Source_End   := Source_Start - 1;

      if Print_Source then
         Source_End   := Source_Start + Max_Src_Length;
      end if;
   end Find_General_Layout;

   -----------------
   -- Find_Status --
   -----------------

   procedure Find_Status
     (FS       : in out File_Name_Type;
      Stamp    : Time_Stamp_Type;
      Checksum : Word;
      Status   : out File_Status)
   is
      Tmp1 : File_Name_Type;
      Tmp2 : File_Name_Type;

   begin
      Tmp1 := Full_Source_Name (FS);

      if Tmp1 = No_File then
         Status := Not_Found;

      elsif File_Stamp (Tmp1) = Stamp then
         FS     := Tmp1;
         Status := OK;

      elsif Checksums_Match (Get_File_Checksum (FS), Checksum) then
         FS := Tmp1;
         Status := Checksum_OK;

      else
         Tmp2 := Matching_Full_Source_Name (FS, Stamp);

         if Tmp2 = No_File then
            Status := Not_Same;
            FS     := Tmp1;

         else
            Status := Not_First_On_PATH;
            FS := Tmp2;
         end if;
      end if;
   end Find_Status;

   -----------
   -- Index --
   -----------

   function Index (Char : Character; Str : String) return Natural is
   begin
      for Index in Str'Range loop
         if Str (Index) = Char then
            return Index;
         end if;
      end loop;

      return 0;
   end Index;

   -------------------
   -- Output_Object --
   -------------------

   procedure Output_Object (O : File_Name_Type) is
      Object_Name : String_Access;
   begin
      if Print_Object then
         Get_Name_String (O);
         Object_Name := To_Host_File_Spec (Name_Buffer (1 .. Name_Len));
         Write_Str (Object_Name.all);
         if Print_Source or else Print_Unit then
            if Too_Long then
               Write_Eol;
               Write_Str ("   ");
            else
               Write_Str (Spaces
                (Object_Start + Object_Name'Length .. Object_End));
            end if;
         end if;
      end if;
   end Output_Object;

   -------------------
   -- Output_Source --
   -------------------

   procedure Output_Source (Sdep_I : Sdep_Id) is
      Stamp       : constant Time_Stamp_Type := Sdep.Table (Sdep_I).Stamp;
      Checksum    : constant Word            := Sdep.Table (Sdep_I).Checksum;
      FS          : File_Name_Type           := Sdep.Table (Sdep_I).Sfile;
      Status      : File_Status;
      Object_Name : String_Access;

   begin
      if Print_Source then
         Find_Status (FS, Stamp, Checksum, Status);
         Get_Name_String (FS);

         Object_Name := To_Host_File_Spec (Name_Buffer (1 .. Name_Len));

         if Verbose_Mode then
            Write_Str ("  Source => ");
            Write_Str (Object_Name.all);

            if not Too_Long then
               Write_Str
                 (Spaces (Source_Start + Object_Name'Length .. Source_End));
            end if;

            Output_Status (Status, Verbose => True);
            Write_Eol;
            Write_Str ("   ");

         else
            if not Selective_Output then
               Output_Status (Status, Verbose => False);
            end if;

            Write_Str (Object_Name.all);
         end if;
      end if;
   end Output_Source;

   -------------------
   -- Output_Status --
   -------------------

   procedure Output_Status (FS : File_Status; Verbose : Boolean) is
   begin
      if Verbose then
         case FS is
            when OK =>
               Write_Str (" unchanged");

            when Checksum_OK =>
               Write_Str (" slightly modified");

            when Not_Found =>
               Write_Str (" file not found");

            when Not_Same =>
               Write_Str (" modified");

            when Not_First_On_PATH =>
               Write_Str (" unchanged version not first on PATH");
         end case;

      else
         case FS is
            when OK =>
               Write_Str ("  OK ");

            when Checksum_OK =>
               Write_Str (" MOK ");

            when Not_Found =>
               Write_Str (" ??? ");

            when Not_Same =>
               Write_Str (" DIF ");

            when Not_First_On_PATH =>
               Write_Str (" HID ");
         end case;
      end if;
   end Output_Status;

   -----------------
   -- Output_Unit --
   -----------------

   procedure Output_Unit (U_Id : Unit_Id) is
      Kind : Character;
      U    : Unit_Record renames Units.Table (U_Id);

   begin
      if Print_Unit then
         Get_Name_String (U.Uname);
         Kind := Name_Buffer (Name_Len);
         Name_Len := Name_Len - 2;

         if not Verbose_Mode then
            Write_Str (Name_Buffer (1 .. Name_Len));

         else
            Write_Str ("Unit => ");
            Write_Eol; Write_Str ("     Name   => ");
            Write_Str (Name_Buffer (1 .. Name_Len));
            Write_Eol; Write_Str ("     Kind   => ");

            if Units.Table (U_Id).Unit_Kind = 'p' then
               Write_Str ("package ");
            else
               Write_Str ("subprogram ");
            end if;

            if Kind = 's' then
               Write_Str ("spec");
            else
               Write_Str ("body");
            end if;
         end if;

         if Verbose_Mode then
            if U.Preelab        or
               U.No_Elab        or
               U.Pure           or
               U.Elaborate_Body or
               U.Remote_Types   or
               U.Shared_Passive or
               U.RCI            or
               U.Predefined
            then
               Write_Eol; Write_Str ("     Flags  =>");

               if U.Preelab then
                  Write_Str (" Preelaborable");
               end if;

               if U.No_Elab then
                  Write_Str (" No_Elab_Code");
               end if;

               if U.Pure then
                  Write_Str (" Pure");
               end if;

               if U.Elaborate_Body then
                  Write_Str (" Elaborate Body");
               end if;

               if U.Remote_Types then
                  Write_Str (" Remote_Types");
               end if;

               if U.Shared_Passive then
                  Write_Str (" Shared_Passive");
               end if;

               if U.Predefined then
                  Write_Str (" Predefined");
               end if;

               if U.RCI then
                  Write_Str (" Remote_Call_Interface");
               end if;
            end if;
         end if;

         if Print_Source then
            if Too_Long then
               Write_Eol; Write_Str ("   ");
            else
               Write_Str (Spaces (Unit_Start + Name_Len + 1 .. Unit_End));
            end if;
         end if;
      end if;
   end Output_Unit;

   -----------------
   -- Reset_Print --
   -----------------

   procedure Reset_Print is
   begin
      if not Selective_Output then
         Selective_Output := True;
         Print_Source := False;
         Print_Object := False;
         Print_Unit   := False;
      end if;
   end Reset_Print;

   -------------------
   -- Scan_Ls_Arg --
   -------------------

   procedure Scan_Ls_Arg (Argv : String; And_Save : Boolean) is
   begin
      pragma Assert (Argv'First = 1);

      if Argv'Length = 0 then
         return;
      end if;

      if Argv (1) = Switch_Character or else Argv (1) = '-' then

         if Argv'Length = 1 then
            Fail ("switch character cannot be followed by a blank");

         --  -I-

         elsif Argv (2 .. Argv'Last) = "I-" then
            Opt.Look_In_Primary_Dir := False;

         --  Forbid  -?-  or  -??-  where ? is any character

         elsif (Argv'Length = 3 and then Argv (3) = '-')
           or else (Argv'Length = 4 and then Argv (4) = '-')
         then
            Fail ("Trailing ""-"" at the end of ", Argv, " forbidden.");

         --  -Idir

         elsif Argv (2) = 'I' then
            Add_Source_Dir (Argv (3 .. Argv'Last), And_Save);
            Add_Lib_Dir (Argv (3 .. Argv'Last), And_Save);

         --  -aIdir (to gcc this is like a -I switch)

         elsif Argv'Length >= 3 and then Argv (2 .. 3) = "aI" then
            Add_Source_Dir (Argv (4 .. Argv'Last), And_Save);

         --  -aOdir

         elsif Argv'Length >= 3 and then Argv (2 .. 3) = "aO" then
            Add_Lib_Dir (Argv (4 .. Argv'Last), And_Save);

         --  -aLdir (to gnatbind this is like a -aO switch)

         elsif Argv'Length >= 3 and then Argv (2 .. 3) = "aL" then
            Add_Lib_Dir (Argv (4 .. Argv'Last), And_Save);

         --  -vPx

         elsif Argv'Length = 4 and then Argv (2 .. 3) = "vP" then
            case Argv (4) is
               when '0' =>
                  Current_Verbosity := Prj.Default;
               when '1' =>
                  Current_Verbosity := Prj.Medium;
               when '2' =>
                  Current_Verbosity := Prj.High;
               when others =>
                  null;
            end case;

         --  -Pproject_file

         elsif Argv'Length >= 3 and then Argv (2) = 'P' then
            if Project_File /= null then
               Fail (Argv & ": second project file forbidden (first is """ &
                     Project_File.all & """)");
            else
               Project_File := new String'(Argv (3 .. Argv'Last));
            end if;

         --  -Xexternal=value

         elsif Argv'Length >= 5 and then Argv (2) = 'X' then
            declare
               Equal_Pos : constant Natural :=
                 Index ('=', Argv (3 .. Argv'Last));
            begin
               if Equal_Pos >= 4 and then
                  Equal_Pos /= Argv'Last then
                  Add (External_Name => Argv (3 .. Equal_Pos - 1),
                       Value => Argv (Equal_Pos + 1 .. Argv'Last));
               else
                  Fail (Argv & " is not a valid external assignment.");
               end if;
            end;

         elsif Argv (2 .. Argv'Last) = "nostdinc" then
            Opt.No_Stdinc := True;

         elsif Argv'Length = 2 then
            case Argv (2) is
               when 'a' => Also_Predef := True;
               when 'h' => Print_Usage := True;
               when 'u' => Reset_Print; Print_Unit   := True;
               when 's' => Reset_Print; Print_Source := True;
               when 'o' => Reset_Print; Print_Object := True;
               when 'v' => Verbose_Mode := True;
               when 'd' => Dependable   := True;
               when others => null;
            end case;
         end if;

      --  If not a switch it must be a file name

      else
         Set_Main_File_Name (Argv);
      end if;
   end Scan_Ls_Arg;

   -----------
   -- Usage --
   -----------

   procedure Usage is
      procedure Write_Switch_Char;
      --  Write two spaces followed by appropriate switch character

      procedure Write_Switch_Char is
      begin
         Write_Str ("  ");
         Write_Char (Switch_Character);
      end Write_Switch_Char;

   --  Start of processing for Usage

   begin
      --  Usage line

      Write_Str ("Usage: ");
      Osint.Write_Program_Name;
      Write_Str ("  switches  [list of object files]");
      Write_Eol;
      Write_Eol;

      --  GNATLS switches

      Write_Str ("switches:");
      Write_Eol;

      --  Line for -a

      Write_Switch_Char;
      Write_Str ("a        also output relevant predefined units");
      Write_Eol;

      --  Line for -u

      Write_Switch_Char;
      Write_Str ("u        output only relevant unit names");
      Write_Eol;

      --  Line for -h

      Write_Switch_Char;
      Write_Str ("h        output this help message");
      Write_Eol;

      --  Line for -s

      Write_Switch_Char;
      Write_Str ("s        output only relevant source names");
      Write_Eol;

      --  Line for -o

      Write_Switch_Char;
      Write_Str ("o        output only relevant object names");
      Write_Eol;

      --  Line for -d

      Write_Switch_Char;
      Write_Str ("d        output sources on which specified units depend");
      Write_Eol;

      --  Line for -v

      Write_Switch_Char;
      Write_Str ("v        verbose output, full path and unit information");
      Write_Eol;
      Write_Eol;

      --  Line for -aI switch

      Write_Switch_Char;
      Write_Str ("aIdir    specify source files search path");
      Write_Eol;

      --  Line for -aO switch

      Write_Switch_Char;
      Write_Str ("aOdir    specify object files search path");
      Write_Eol;

      --  Line for -I switch

      Write_Switch_Char;
      Write_Str ("Idir     like -aIdir -aOdir");
      Write_Eol;

      --  Line for -I- switch

      Write_Switch_Char;
      Write_Str ("I-       do not look for sources & object files");
      Write_Str (" in the default directory");
      Write_Eol;

      --  Line for -vPx

      Write_Switch_Char;
      Write_Str ("vPx      verbosity for project file (0, 1 or 2)");
      Write_Eol;

      --  Line for -Pproject_file

      Write_Switch_Char;
      Write_Str ("Pprj     use a project file prj");
      Write_Eol;

      --  Line for -Xexternal=value

      Write_Switch_Char;
      Write_Str ("Xext=val specify an external value.");
      Write_Eol;

      --  Line for -nostdinc

      Write_Switch_Char;
      Write_Str ("nostdinc do not look for source files");
      Write_Str (" in the system default directory");
      Write_Eol;

      --  File Status explanation

      Write_Eol;
      Write_Str (" file status can be:");
      Write_Eol;

      for ST in File_Status loop
         Write_Str ("   ");
         Output_Status (ST, Verbose => False);
         Write_Str (" ==> ");
         Output_Status (ST, Verbose => True);
         Write_Eol;
      end loop;

   end Usage;

   --   Start of processing for Gnatls

begin
   Osint.Initialize (Binder);

   Namet.Initialize;
   Csets.Initialize;

   Snames.Initialize;

   Prj.Initialize;

   --  Use low level argument routines to avoid dragging in the secondary stack

   Next_Arg := 1;

   Scan_Args : while Next_Arg < Arg_Count loop
      declare
         Next_Argv : String (1 .. Len_Arg (Next_Arg));

      begin
         Fill_Arg (Next_Argv'Address, Next_Arg);
         Scan_Ls_Arg (Next_Argv, And_Save => True);
      end;

      Next_Arg := Next_Arg + 1;
   end loop Scan_Args;

   --  If a switch -P is used, parse the project file

   if Project_File /= null then

      Prj.Pars.Set_Verbosity (To => Current_Verbosity);

      Prj.Pars.Parse
        (Project           => Project,
         Project_File_Name => Project_File.all);

      if Project = Prj.No_Project then
         Fail ("""" & Project_File.all & """ processing failed");
      end if;

      --  Add the source directories and the object directories
      --  to the searched directories.

      declare
         procedure Register_Source_Dirs is new
           Prj.Env.For_All_Source_Dirs (Add_Src_Search_Dir);

         procedure Register_Object_Dirs is new
           Prj.Env.For_All_Object_Dirs (Add_Lib_Search_Dir);

      begin
         Register_Source_Dirs (Project);
         Register_Object_Dirs (Project);
      end;

      --  Check if a package gnatls is in the project file and if there is
      --  there is one, get the switches, if any, and scan them.

      declare
         Data       : Prj.Project_Data := Prj.Projects.Table (Project);
         Pkg        : Prj.Package_Id :=
                        Prj.Util.Value_Of
                          (Name        => Name_Gnatls,
                           In_Packages => Data.Decl.Packages);
         Element    : Package_Element;
         Switches   : Prj.Variable_Value;
         Current    : Prj.String_List_Id;
         The_String : String_Element;

      begin
         if Pkg /= No_Package then
            Element := Packages.Table (Pkg);
            Switches :=
              Prj.Util.Value_Of
                (Variable_Name => Name_Switches,
                 In_Variables => Element.Decl.Attributes);

            case Switches.Kind is
               when Prj.Undefined =>
                  null;

               when Prj.Single =>
                  if String_Length (Switches.Value) > 0 then
                     String_To_Name_Buffer (Switches.Value);
                     Scan_Ls_Arg
                       (Name_Buffer (1 .. Name_Len),
                        And_Save => False);
                  end if;

               when Prj.List =>
                  Current := Switches.Values;
                  while Current /= Prj.Nil_String loop
                     The_String := String_Elements.Table (Current);

                     if String_Length (The_String.Value) > 0 then
                        String_To_Name_Buffer (The_String.Value);
                        Scan_Ls_Arg
                          (Name_Buffer (1 .. Name_Len),
                           And_Save => False);
                     end if;

                     Current := The_String.Next;
                  end loop;
            end case;
         end if;
      end;
   end if;

   --  Add the source and object directories specified on the
   --  command line, if any, to the searched directories.

   while First_Source_Dir /= null loop
      Add_Src_Search_Dir (First_Source_Dir.Value.all);
      First_Source_Dir := First_Source_Dir.Next;
   end loop;

   while First_Lib_Dir /= null loop
      Add_Lib_Search_Dir (First_Lib_Dir.Value.all);
      First_Lib_Dir := First_Lib_Dir.Next;
   end loop;

   --  Finally, add the default directories.

   Osint.Add_Default_Search_Dirs;

   if Verbose_Mode then

      --  WARNING: the output of gnatls -v is used during the compilation
      --  and installation of GLADE to recreate sdefault.adb and locate
      --  the libgnat.a to use. Any change in the output of gnatls -v must
      --  be synchronized with the GLADE Dist/config.sdefault shell script.

      Write_Eol;
      Write_Str ("GNATLS ");
      Write_Str (Gnat_Version_String);
      Write_Str (" Copyright 1997-2001 Free Software Foundation, Inc.");
      Write_Eol;
      Write_Eol;
      Write_Str ("Source Search Path:");
      Write_Eol;

      for J in 1 .. Nb_Dir_In_Src_Search_Path loop
         Write_Str ("   ");

         if Dir_In_Src_Search_Path (J)'Length = 0 then
            Write_Str ("<Current_Directory>");
         else
            Write_Str (To_Host_Dir_Spec
              (Dir_In_Src_Search_Path (J).all, True).all);
         end if;

         Write_Eol;
      end loop;

      Write_Eol;
      Write_Eol;
      Write_Str ("Object Search Path:");
      Write_Eol;

      for J in 1 .. Nb_Dir_In_Obj_Search_Path loop
         Write_Str ("   ");

         if Dir_In_Obj_Search_Path (J)'Length = 0 then
            Write_Str ("<Current_Directory>");
         else
            Write_Str (To_Host_Dir_Spec
              (Dir_In_Obj_Search_Path (J).all, True).all);
         end if;

         Write_Eol;
      end loop;

      Write_Eol;
   end if;

   --  Output usage information when requested

   if Print_Usage then
      Usage;
   end if;

   if not More_Lib_Files then
      if not Print_Usage and then not Verbose_Mode then
         Usage;
      end if;

      Exit_Program (E_Fatal);
   end if;

   Initialize_ALI;
   Initialize_ALI_Source;

   --  Print out all library for which no ALI files can be located

   while More_Lib_Files loop
      Main_File := Next_Main_Lib_File;
      Ali_File := Full_Lib_File_Name (Lib_File_Name (Main_File));

      if Ali_File = No_File then
         Write_Str ("Can't find library info for ");
         Get_Decoded_Name_String (Main_File);
         Write_Char ('"');
         Write_Str (Name_Buffer (1 .. Name_Len));
         Write_Char ('"');
         Write_Eol;

      else
         Ali_File := Strip_Directory (Ali_File);

         if Get_Name_Table_Info (Ali_File) = 0 then
            Text := Read_Library_Info (Ali_File, True);
            Id :=
              Scan_ALI
                (Ali_File, Text, Ignore_ED => False, Err => False);
            Free (Text);
         end if;
      end if;
   end loop;

   Find_General_Layout;
   for Id in ALIs.First .. ALIs.Last loop
      declare
         Last_U : Unit_Id;

      begin
         Get_Name_String (Units.Table (ALIs.Table (Id).First_Unit).Uname);

         if Also_Predef or else not Is_Internal_Unit then
            Output_Object (ALIs.Table (Id).Ofile_Full_Name);

            --  In verbose mode print all main units in the ALI file, otherwise
            --  just print the first one to ease columnwise printout

            if Verbose_Mode then
               Last_U := ALIs.Table (Id).Last_Unit;
            else
               Last_U := ALIs.Table (Id).First_Unit;
            end if;

            for U in ALIs.Table (Id).First_Unit .. Last_U loop
               if U /= ALIs.Table (Id).First_Unit
                 and then Selective_Output
                 and then Print_Unit
               then
                  Write_Eol;
               end if;

               Output_Unit (U);

               --  Output source now, unless if it will be done as part of
               --  outputing dependencies.

               if not (Dependable and then Print_Source) then
                  Output_Source (Corresponding_Sdep_Entry (Id, U));
               end if;
            end loop;

            --  Print out list of dependable units

            if Dependable and then Print_Source then
               if Verbose_Mode then
                  Write_Str ("depends upon");
                  Write_Eol;
                  Write_Str ("   ");

               else
                  Write_Eol;
               end if;

               for D in
                 ALIs.Table (Id).First_Sdep .. ALIs.Table (Id).Last_Sdep
               loop
                  if Also_Predef
                    or else not Is_Internal_File_Name (Sdep.Table (D).Sfile)
                  then
                     if Verbose_Mode then
                        Write_Str ("   ");
                        Output_Source (D);
                     elsif Too_Long then
                        Write_Str ("   ");
                        Output_Source (D);
                        Write_Eol;
                     else
                        Write_Str (Spaces (1 .. Source_Start - 2));
                        Output_Source (D);
                        Write_Eol;
                     end if;
                  end if;
               end loop;
            end if;

            Write_Eol;
         end if;
      end;
   end loop;

   --  All done. Set proper exit status.

   Namet.Finalize;
   Exit_Program (E_Success);

end Gnatls;
