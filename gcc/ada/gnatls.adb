------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               G N A T L S                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2013, Free Software Foundation, Inc.         --
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

with ALI;         use ALI;
with ALI.Util;    use ALI.Util;
with Binderr;     use Binderr;
with Butil;       use Butil;
with Csets;       use Csets;
with Fname;       use Fname;
with Gnatvsn;     use Gnatvsn;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Namet;       use Namet;
with Opt;         use Opt;
with Osint;       use Osint;
with Osint.L;     use Osint.L;
with Output;      use Output;
with Prj.Env;     use Prj.Env;
with Rident;      use Rident;
with Sdefault;
with Snames;
with Switch;      use Switch;
with Types;       use Types;

with GNAT.Case_Util; use GNAT.Case_Util;

procedure Gnatls is
   pragma Ident (Gnat_Static_Version_String);

   --  NOTE : The following string may be used by other tools, such as GPS. So
   --  it can only be modified if these other uses are checked and coordinated.

   Project_Search_Path : constant String := "Project Search Path:";
   --  Label displayed in verbose mode before the directories in the project
   --  search path. Do not modify without checking NOTE above.

   Prj_Path : Prj.Env.Project_Search_Path;

   Max_Column : constant := 80;

   No_Obj : aliased String := "<no_obj>";

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
   --  Simply linked list of dirs

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
   Text      : Text_Buffer_Ptr;
   Next_Arg  : Positive;

   Too_Long : Boolean := False;
   --  When True, lines are too long for multi-column output and each
   --  item of information is on a different line.

   Selective_Output : Boolean := False;
   Print_Usage      : Boolean := False;
   Print_Unit       : Boolean := True;
   Print_Source     : Boolean := True;
   Print_Object     : Boolean := True;
   --  Flags controlling the form of the output

   Also_Predef       : Boolean := False;  --  -a
   Dependable        : Boolean := False;  --  -d
   License           : Boolean := False;  --  -l
   Very_Verbose_Mode : Boolean := False;  --  -V
   --  Command line flags

   Unit_Start   : Integer;
   Unit_End     : Integer;
   Source_Start : Integer;
   Source_End   : Integer;
   Object_Start : Integer;
   Object_End   : Integer;
   --  Various column starts and ends

   Spaces : constant String (1 .. Max_Column) := (others => ' ');

   RTS_Specified : String_Access := null;
   --  Used to detect multiple use of --RTS= switch

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Add_Lib_Dir (Dir : String);
   --  Add an object directory in the list First_Lib_Dir-Last_Lib_Dir

   procedure Add_Source_Dir (Dir : String);
   --  Add a source directory in the list First_Source_Dir-Last_Source_Dir

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
   --  Give the Sdep entry corresponding to the unit U in ali record A

   procedure Output_Object (O : File_Name_Type);
   --  Print out the name of the object when requested

   procedure Output_Source (Sdep_I : Sdep_Id);
   --  Print out the name and status of the source corresponding to this
   --  sdep entry.

   procedure Output_Status (FS : File_Status; Verbose : Boolean);
   --  Print out FS either in a coded form if verbose is false or in an
   --  expanded form otherwise.

   procedure Output_Unit (ALI : ALI_Id; U_Id : Unit_Id);
   --  Print out information on the unit when requested

   procedure Reset_Print;
   --  Reset Print flags properly when selective output is chosen

   procedure Scan_Ls_Arg (Argv : String);
   --  Scan and process lser specific arguments. Argv is a single argument

   procedure Search_RTS (Name : String);
   --  Find include and objects path for the RTS name.

   procedure Usage;
   --  Print usage message

   procedure Output_License_Information;
   --  Output license statement, and if not found, output reference to
   --  COPYING.

   function Image (Restriction : Restriction_Id) return String;
   --  Returns the capitalized image of Restriction

   ------------------------------------------
   -- GNATDIST specific output subprograms --
   ------------------------------------------

   package GNATDIST is

      --  Any modification to this subunit requires synchronization with the
      --  GNATDIST sources.

      procedure Output_ALI (A : ALI_Id);
      --  Comment required saying what this routine does ???

      procedure Output_No_ALI (Afile : File_Name_Type);
      --  Comments required saying what this routine does ???

   end GNATDIST;

   -----------------
   -- Add_Lib_Dir --
   -----------------

   procedure Add_Lib_Dir (Dir : String) is
   begin
      if First_Lib_Dir = null then
         First_Lib_Dir :=
           new Dir_Data'
             (Value => new String'(Dir),
              Next  => null);
         Last_Lib_Dir := First_Lib_Dir;

      else
         Last_Lib_Dir.Next :=
           new Dir_Data'
             (Value => new String'(Dir),
              Next  => null);
         Last_Lib_Dir := Last_Lib_Dir.Next;
      end if;
   end Add_Lib_Dir;

   --------------------
   -- Add_Source_Dir --
   --------------------

   procedure Add_Source_Dir (Dir : String) is
   begin
      if First_Source_Dir = null then
         First_Source_Dir :=
           new Dir_Data'
             (Value => new String'(Dir),
              Next  => null);
         Last_Source_Dir := First_Source_Dir;

      else
         Last_Source_Dir.Next :=
           new Dir_Data'
             (Value => new String'(Dir),
              Next  => null);
         Last_Source_Dir := Last_Source_Dir.Next;
      end if;
   end Add_Source_Dir;

   ------------------------------
   -- Corresponding_Sdep_Entry --
   ------------------------------

   function Corresponding_Sdep_Entry
     (A : ALI_Id;
      U : Unit_Id) return Sdep_Id
   is
   begin
      for D in ALIs.Table (A).First_Sdep .. ALIs.Table (A).Last_Sdep loop
         if Sdep.Table (D).Sfile = Units.Table (U).Sfile then
            return D;
         end if;
      end loop;

      Error_Msg_Unit_1 := Units.Table (U).Uname;
      Error_Msg_File_1 := ALIs.Table (A).Afile;
      Write_Eol;
      Error_Msg ("wrong ALI format, can't find dependency line for $ in {");
      Exit_Program (E_Fatal);
      return No_Sdep_Id;
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
               if ALIs.Table (Id).No_Object then
                  Max_Obj_Length :=
                    Integer'Max (Max_Obj_Length, No_Obj'Length);
               else
                  Get_Name_String (ALIs.Table (Id).Ofile_Full_Name);
                  Max_Obj_Length := Integer'Max (Max_Obj_Length, Name_Len + 1);
               end if;
            end if;
         end if;
      end loop;

      --  Verify is output is not wider than maximum number of columns

      Too_Long :=
        Verbose_Mode
          or else
            (Max_Unit_Length + Max_Src_Length + Max_Obj_Length) > Max_Column;

      --  Set start and end of columns

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

      Source_End := Source_Start - 1;

      if Print_Source then
         Source_End := Source_Start + Max_Src_Length;
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

   --------------
   -- GNATDIST --
   --------------

   package body GNATDIST is

      N_Flags   : Natural;
      N_Indents : Natural := 0;

      type Token_Type is
        (T_No_ALI,
         T_ALI,
         T_Unit,
         T_With,
         T_Source,
         T_Afile,
         T_Ofile,
         T_Sfile,
         T_Name,
         T_Main,
         T_Kind,
         T_Flags,
         T_Preelaborated,
         T_Pure,
         T_Has_RACW,
         T_Remote_Types,
         T_Shared_Passive,
         T_RCI,
         T_Predefined,
         T_Internal,
         T_Is_Generic,
         T_Procedure,
         T_Function,
         T_Package,
         T_Subprogram,
         T_Spec,
         T_Body);

      Image : constant array (Token_Type) of String_Access :=
                (T_No_ALI         => new String'("No_ALI"),
                 T_ALI            => new String'("ALI"),
                 T_Unit           => new String'("Unit"),
                 T_With           => new String'("With"),
                 T_Source         => new String'("Source"),
                 T_Afile          => new String'("Afile"),
                 T_Ofile          => new String'("Ofile"),
                 T_Sfile          => new String'("Sfile"),
                 T_Name           => new String'("Name"),
                 T_Main           => new String'("Main"),
                 T_Kind           => new String'("Kind"),
                 T_Flags          => new String'("Flags"),
                 T_Preelaborated  => new String'("Preelaborated"),
                 T_Pure           => new String'("Pure"),
                 T_Has_RACW       => new String'("Has_RACW"),
                 T_Remote_Types   => new String'("Remote_Types"),
                 T_Shared_Passive => new String'("Shared_Passive"),
                 T_RCI            => new String'("RCI"),
                 T_Predefined     => new String'("Predefined"),
                 T_Internal       => new String'("Internal"),
                 T_Is_Generic     => new String'("Is_Generic"),
                 T_Procedure      => new String'("procedure"),
                 T_Function       => new String'("function"),
                 T_Package        => new String'("package"),
                 T_Subprogram     => new String'("subprogram"),
                 T_Spec           => new String'("spec"),
                 T_Body           => new String'("body"));

      procedure Output_Name  (N : Name_Id);
      --  Remove any encoding info (%b and %s) and output N

      procedure Output_Afile (A : File_Name_Type);
      procedure Output_Ofile (O : File_Name_Type);
      procedure Output_Sfile (S : File_Name_Type);
      --  Output various names. Check that the name is different from no name.
      --  Otherwise, skip the output.

      procedure Output_Token (T : Token_Type);
      --  Output token using specific format. That is several indentations and:
      --
      --  T_No_ALI  .. T_With : <token> & " =>" & NL
      --  T_Source  .. T_Kind : <token> & " => "
      --  T_Flags             : <token> & " =>"
      --  T_Preelab .. T_Body : " " & <token>

      procedure Output_Sdep  (S : Sdep_Id);
      procedure Output_Unit  (U : Unit_Id);
      procedure Output_With  (W : With_Id);
      --  Output this entry as a global section (like ALIs)

      ------------------
      -- Output_Afile --
      ------------------

      procedure Output_Afile (A : File_Name_Type) is
      begin
         if A /= No_File then
            Output_Token (T_Afile);
            Write_Name (A);
            Write_Eol;
         end if;
      end Output_Afile;

      ----------------
      -- Output_ALI --
      ----------------

      procedure Output_ALI (A : ALI_Id) is
      begin
         Output_Token (T_ALI);
         N_Indents := N_Indents + 1;

         Output_Afile (ALIs.Table (A).Afile);
         Output_Ofile (ALIs.Table (A).Ofile_Full_Name);
         Output_Sfile (ALIs.Table (A).Sfile);

         --  Output Main

         if ALIs.Table (A).Main_Program /= None then
            Output_Token (T_Main);

            if ALIs.Table (A).Main_Program = Proc then
               Output_Token (T_Procedure);
            else
               Output_Token (T_Function);
            end if;

            Write_Eol;
         end if;

         --  Output Units

         for U in ALIs.Table (A).First_Unit .. ALIs.Table (A).Last_Unit loop
            Output_Unit (U);
         end loop;

         --  Output Sdeps

         for S in ALIs.Table (A).First_Sdep .. ALIs.Table (A).Last_Sdep loop
            Output_Sdep (S);
         end loop;

         N_Indents := N_Indents - 1;
      end Output_ALI;

      -------------------
      -- Output_No_ALI --
      -------------------

      procedure Output_No_ALI (Afile : File_Name_Type) is
      begin
         Output_Token (T_No_ALI);
         N_Indents := N_Indents + 1;
         Output_Afile (Afile);
         N_Indents := N_Indents - 1;
      end Output_No_ALI;

      -----------------
      -- Output_Name --
      -----------------

      procedure Output_Name (N : Name_Id) is
      begin
         --  Remove any encoding info (%s or %b)

         Get_Name_String (N);

         if Name_Len > 2
           and then Name_Buffer (Name_Len - 1) = '%'
         then
            Name_Len := Name_Len - 2;
         end if;

         Output_Token (T_Name);
         Write_Str (Name_Buffer (1 .. Name_Len));
         Write_Eol;
      end Output_Name;

      ------------------
      -- Output_Ofile --
      ------------------

      procedure Output_Ofile (O : File_Name_Type) is
      begin
         if O /= No_File then
            Output_Token (T_Ofile);
            Write_Name (O);
            Write_Eol;
         end if;
      end Output_Ofile;

      -----------------
      -- Output_Sdep --
      -----------------

      procedure Output_Sdep (S : Sdep_Id) is
      begin
         Output_Token (T_Source);
         Write_Name (Sdep.Table (S).Sfile);
         Write_Eol;
      end Output_Sdep;

      ------------------
      -- Output_Sfile --
      ------------------

      procedure Output_Sfile (S : File_Name_Type) is
         FS : File_Name_Type := S;

      begin
         if FS /= No_File then

            --  We want to output the full source name

            FS := Full_Source_Name (FS);

            --  There is no full source name. This occurs for instance when a
            --  withed unit has a spec file but no body file. This situation is
            --  not a problem for GNATDIST since the unit may be located on a
            --  partition we do not want to build. However, we need to locate
            --  the spec file and to find its full source name. Replace the
            --  body file name with the spec file name used to compile the
            --  current unit when possible.

            if FS = No_File then
               Get_Name_String (S);

               if Name_Len > 4
                 and then Name_Buffer (Name_Len - 3 .. Name_Len) = ".adb"
               then
                  Name_Buffer (Name_Len) := 's';
                  FS := Full_Source_Name (Name_Find);
               end if;
            end if;
         end if;

         if FS /= No_File then
            Output_Token (T_Sfile);
            Write_Name (FS);
            Write_Eol;
         end if;
      end Output_Sfile;

      ------------------
      -- Output_Token --
      ------------------

      procedure Output_Token (T : Token_Type) is
      begin
         if T in T_No_ALI .. T_Flags then
            for J in 1 .. N_Indents loop
               Write_Str ("   ");
            end loop;

            Write_Str (Image (T).all);

            for J in Image (T)'Length .. 12 loop
               Write_Char (' ');
            end loop;

            Write_Str ("=>");

            if T in T_No_ALI .. T_With then
               Write_Eol;
            elsif T in T_Source .. T_Name then
               Write_Char (' ');
            end if;

         elsif T in T_Preelaborated .. T_Body then
            if T in T_Preelaborated .. T_Is_Generic then
               if N_Flags = 0 then
                  Output_Token (T_Flags);
               end if;

               N_Flags := N_Flags + 1;
            end if;

            Write_Char (' ');
            Write_Str  (Image (T).all);

         else
            Write_Str  (Image (T).all);
         end if;
      end Output_Token;

      -----------------
      -- Output_Unit --
      -----------------

      procedure Output_Unit (U : Unit_Id) is
      begin
         Output_Token (T_Unit);
         N_Indents := N_Indents + 1;

         --  Output Name

         Output_Name (Name_Id (Units.Table (U).Uname));

         --  Output Kind

         Output_Token (T_Kind);

         if Units.Table (U).Unit_Kind = 'p' then
            Output_Token (T_Package);
         else
            Output_Token (T_Subprogram);
         end if;

         if Name_Buffer (Name_Len) = 's' then
            Output_Token (T_Spec);
         else
            Output_Token (T_Body);
         end if;

         Write_Eol;

         --  Output source file name

         Output_Sfile (Units.Table (U).Sfile);

         --  Output Flags

         N_Flags := 0;

         if Units.Table (U).Preelab then
            Output_Token (T_Preelaborated);
         end if;

         if Units.Table (U).Pure then
            Output_Token (T_Pure);
         end if;

         if Units.Table (U).Has_RACW then
            Output_Token (T_Has_RACW);
         end if;

         if Units.Table (U).Remote_Types then
            Output_Token (T_Remote_Types);
         end if;

         if Units.Table (U).Shared_Passive then
            Output_Token (T_Shared_Passive);
         end if;

         if Units.Table (U).RCI then
            Output_Token (T_RCI);
         end if;

         if Units.Table (U).Predefined then
            Output_Token (T_Predefined);
         end if;

         if Units.Table (U).Internal then
            Output_Token (T_Internal);
         end if;

         if Units.Table (U).Is_Generic then
            Output_Token (T_Is_Generic);
         end if;

         if N_Flags > 0 then
            Write_Eol;
         end if;

         --  Output Withs

         for W in Units.Table (U).First_With .. Units.Table (U).Last_With loop
            Output_With (W);
         end loop;

         N_Indents := N_Indents - 1;
      end Output_Unit;

      -----------------
      -- Output_With --
      -----------------

      procedure Output_With (W : With_Id) is
      begin
         Output_Token (T_With);
         N_Indents := N_Indents + 1;

         Output_Name (Name_Id (Withs.Table (W).Uname));

         --  Output Kind

         Output_Token (T_Kind);

         if Name_Buffer (Name_Len) = 's' then
            Output_Token (T_Spec);
         else
            Output_Token (T_Body);
         end if;

         Write_Eol;

         Output_Afile (Withs.Table (W).Afile);
         Output_Sfile (Withs.Table (W).Sfile);

         N_Indents := N_Indents - 1;
      end Output_With;

   end GNATDIST;

   -----------
   -- Image --
   -----------

   function Image (Restriction : Restriction_Id) return String is
      Result : String := Restriction'Img;
      Skip   : Boolean := True;

   begin
      for J in Result'Range loop
         if Skip then
            Skip := False;
            Result (J) := To_Upper (Result (J));

         elsif Result (J) = '_' then
            Skip := True;

         else
            Result (J) := To_Lower (Result (J));
         end if;
      end loop;

      return Result;
   end Image;

   --------------------------------
   -- Output_License_Information --
   --------------------------------

   procedure Output_License_Information is
   begin
      case Build_Type is
         when others =>
            Write_Str ("Please refer to file COPYING in your distribution"
                     & " for license terms.");
            Write_Eol;
      end case;

      Exit_Program (E_Success);
   end Output_License_Information;

   -------------------
   -- Output_Object --
   -------------------

   procedure Output_Object (O : File_Name_Type) is
      Object_Name : String_Access;

   begin
      if Print_Object then
         if O /= No_File then
            Get_Name_String (O);
            Object_Name := To_Host_File_Spec (Name_Buffer (1 .. Name_Len));
         else
            Object_Name := No_Obj'Unchecked_Access;
         end if;

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
      Stamp       : Time_Stamp_Type;
      Checksum    : Word;
      FS          : File_Name_Type;
      Status      : File_Status;
      Object_Name : String_Access;

   begin
      if Sdep_I = No_Sdep_Id then
         return;
      end if;

      Stamp    := Sdep.Table (Sdep_I).Stamp;
      Checksum := Sdep.Table (Sdep_I).Checksum;
      FS       := Sdep.Table (Sdep_I).Sfile;

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

   procedure Output_Unit (ALI : ALI_Id; U_Id : Unit_Id) is
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
            Write_Eol;
            Write_Str ("     Name   => ");
            Write_Str (Name_Buffer (1 .. Name_Len));
            Write_Eol;
            Write_Str ("     Kind   => ");

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
            if U.Preelab             or else
               U.No_Elab             or else
               U.Pure                or else
               U.Dynamic_Elab        or else
               U.Has_RACW            or else
               U.Remote_Types        or else
               U.Shared_Passive      or else
               U.RCI                 or else
               U.Predefined          or else
               U.Internal            or else
               U.Is_Generic          or else
               U.Init_Scalars        or else
               U.SAL_Interface       or else
               U.Body_Needed_For_SAL or else
               U.Elaborate_Body
            then
               Write_Eol;
               Write_Str ("     Flags  =>");

               if U.Preelab then
                  Write_Str (" Preelaborable");
               end if;

               if U.No_Elab then
                  Write_Str (" No_Elab_Code");
               end if;

               if U.Pure then
                  Write_Str (" Pure");
               end if;

               if U.Dynamic_Elab then
                  Write_Str (" Dynamic_Elab");
               end if;

               if U.Has_RACW then
                  Write_Str (" Has_RACW");
               end if;

               if U.Remote_Types then
                  Write_Str (" Remote_Types");
               end if;

               if U.Shared_Passive then
                  Write_Str (" Shared_Passive");
               end if;

               if U.RCI then
                  Write_Str (" RCI");
               end if;

               if U.Predefined then
                  Write_Str (" Predefined");
               end if;

               if U.Internal then
                  Write_Str (" Internal");
               end if;

               if U.Is_Generic then
                  Write_Str (" Is_Generic");
               end if;

               if U.Init_Scalars then
                  Write_Str (" Init_Scalars");
               end if;

               if U.SAL_Interface then
                  Write_Str (" SAL_Interface");
               end if;

               if U.Body_Needed_For_SAL then
                  Write_Str (" Body_Needed_For_SAL");
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
            end if;

            declare
               Restrictions : constant Restrictions_Info :=
                                ALIs.Table (ALI).Restrictions;

            begin
               --  If the source was compiled with pragmas Restrictions,
               --  Display these restrictions.

               if Restrictions.Set /= (All_Restrictions => False) then
                  Write_Eol;
                  Write_Str ("     pragma Restrictions  =>");

                  --  For boolean restrictions, just display the name of the
                  --  restriction; for valued restrictions, also display the
                  --  restriction value.

                  for Restriction in All_Restrictions loop
                     if Restrictions.Set (Restriction) then
                        Write_Eol;
                        Write_Str ("       ");
                        Write_Str (Image (Restriction));

                        if Restriction in All_Parameter_Restrictions then
                           Write_Str (" =>");
                           Write_Str (Restrictions.Value (Restriction)'Img);
                        end if;
                     end if;
                  end loop;
               end if;

               --  If the unit violates some Restrictions, display the list of
               --  these restrictions.

               if Restrictions.Violated /= (All_Restrictions => False) then
                  Write_Eol;
                  Write_Str ("     Restrictions violated =>");

                  --  For boolean restrictions, just display the name of the
                  --  restriction. For valued restrictions, also display the
                  --  restriction value.

                  for Restriction in All_Restrictions loop
                     if Restrictions.Violated (Restriction) then
                        Write_Eol;
                        Write_Str ("       ");
                        Write_Str (Image (Restriction));

                        if Restriction in All_Parameter_Restrictions then
                           if Restrictions.Count (Restriction) > 0 then
                              Write_Str (" =>");

                              if Restrictions.Unknown (Restriction) then
                                 Write_Str (" at least");
                              end if;

                              Write_Str (Restrictions.Count (Restriction)'Img);
                           end if;
                        end if;
                     end if;
                  end loop;
               end if;
            end;
         end if;

         if Print_Source then
            if Too_Long then
               Write_Eol;
               Write_Str ("   ");
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

   ----------------
   -- Search_RTS --
   ----------------

   procedure Search_RTS (Name : String) is
      Src_Path : String_Ptr;
      Lib_Path : String_Ptr;
      --  Paths for source and include subdirs

      Rts_Full_Path : String_Access;
      --  Full path for RTS project

   begin
      --  Try to find the RTS

      Src_Path := Get_RTS_Search_Dir (Name, Include);
      Lib_Path := Get_RTS_Search_Dir (Name, Objects);

      --  For non-project RTS, both the include and the objects directories
      --  must be present.

      if Src_Path /= null and then Lib_Path /= null then
         Add_Search_Dirs (Src_Path, Include);
         Add_Search_Dirs (Lib_Path, Objects);
         return;
      end if;

      if Lib_Path /= null then
         Osint.Fail ("RTS path not valid: missing adainclude directory");
      elsif Src_Path /= null then
         Osint.Fail ("RTS path not valid: missing adalib directory");
      end if;

      --  Try to find the RTS on the project path. First setup the project path

      Initialize_Default_Project_Path
        (Prj_Path, Target_Name => Sdefault.Target_Name.all);

      Rts_Full_Path := Get_Runtime_Path (Prj_Path, Name);

      if Rts_Full_Path /= null then

         --  Directory name was found on the project path. Look for the
         --  include subdirectory(s).

         Src_Path := Get_RTS_Search_Dir (Rts_Full_Path.all, Include);

         if Src_Path /= null then
            Add_Search_Dirs (Src_Path, Include);

            --  Add the lib subdirectory if it exists

            Lib_Path := Get_RTS_Search_Dir (Rts_Full_Path.all, Objects);

            if Lib_Path /= null then
               Add_Search_Dirs (Lib_Path, Objects);
            end if;

            return;
         end if;
      end if;

      Osint.Fail
        ("RTS path not valid: missing adainclude and adalib directories");
   end Search_RTS;

   -------------------
   -- Scan_Ls_Arg --
   -------------------

   procedure Scan_Ls_Arg (Argv : String) is
      FD  : File_Descriptor;
      Len : Integer;
      OK  : Boolean;

   begin
      pragma Assert (Argv'First = 1);

      if Argv'Length = 0 then
         return;
      end if;

      OK := True;
      if Argv (1) = '-' then
         if Argv'Length = 1 then
            Fail ("switch character cannot be followed by a blank");

         --  Processing for -I-

         elsif Argv (2 .. Argv'Last) = "I-" then
            Opt.Look_In_Primary_Dir := False;

         --  Forbid -?- or -??- where ? is any character

         elsif (Argv'Length = 3 and then Argv (3) = '-')
           or else (Argv'Length = 4 and then Argv (4) = '-')
         then
            Fail ("Trailing ""-"" at the end of " & Argv & " forbidden.");

         --  Processing for -Idir

         elsif Argv (2) = 'I' then
            Add_Source_Dir (Argv (3 .. Argv'Last));
            Add_Lib_Dir (Argv (3 .. Argv'Last));

         --  Processing for -aIdir (to gcc this is like a -I switch)

         elsif Argv'Length >= 3 and then Argv (2 .. 3) = "aI" then
            Add_Source_Dir (Argv (4 .. Argv'Last));

         --  Processing for -aOdir

         elsif Argv'Length >= 3 and then Argv (2 .. 3) = "aO" then
            Add_Lib_Dir (Argv (4 .. Argv'Last));

         --  Processing for -aLdir (to gnatbind this is like a -aO switch)

         elsif Argv'Length >= 3 and then Argv (2 .. 3) = "aL" then
            Add_Lib_Dir (Argv (4 .. Argv'Last));

         --  Processing for -aP<dir>

         elsif Argv'Length > 3 and then Argv (1 .. 3) = "-aP" then
            Add_Directories (Prj_Path, Argv (4 .. Argv'Last));

         --  Processing for -nostdinc

         elsif Argv (2 .. Argv'Last) = "nostdinc" then
            Opt.No_Stdinc := True;

         --  Processing for one character switches

         elsif Argv'Length = 2 then
            case Argv (2) is
               when 'a' => Also_Predef               := True;
               when 'h' => Print_Usage               := True;
               when 'u' => Reset_Print; Print_Unit   := True;
               when 's' => Reset_Print; Print_Source := True;
               when 'o' => Reset_Print; Print_Object := True;
               when 'v' => Verbose_Mode              := True;
               when 'd' => Dependable                := True;
               when 'l' => License                   := True;
               when 'V' => Very_Verbose_Mode         := True;

               when others => OK := False;
            end case;

         --  Processing for -files=file

         elsif Argv'Length > 7 and then Argv (1 .. 7) = "-files=" then
            FD := Open_Read (Argv (8 .. Argv'Last), GNAT.OS_Lib.Text);

            if FD = Invalid_FD then
               Osint.Fail ("could not find text file """ &
                           Argv (8 .. Argv'Last) & '"');
            end if;

            Len := Integer (File_Length (FD));

            declare
               Buffer : String (1 .. Len + 1);
               Index  : Positive := 1;
               Last   : Positive;

            begin
               --  Read the file

               Len := Read (FD, Buffer (1)'Address, Len);
               Buffer (Buffer'Last) := ASCII.NUL;
               Close (FD);

               --  Scan the file line by line

               while Index < Buffer'Last loop

                  --  Find the end of line

                  Last := Index;
                  while Last <= Buffer'Last
                    and then Buffer (Last) /= ASCII.LF
                    and then Buffer (Last) /= ASCII.CR
                  loop
                     Last := Last + 1;
                  end loop;

                  --  Ignore empty lines

                  if Last > Index then
                     Add_File (Buffer (Index .. Last - 1));
                  end if;

                  --  Find the beginning of the next line

                  Index := Last;
                  while Buffer (Index) = ASCII.CR or else
                        Buffer (Index) = ASCII.LF
                  loop
                     Index := Index + 1;
                  end loop;
               end loop;
            end;

         --  Processing for --RTS=path

         elsif Argv'Length >= 5 and then Argv (1 .. 5) = "--RTS" then
            if Argv'Length <= 6 or else Argv (6) /= '='then
               Osint.Fail ("missing path for --RTS");

            else
               --  Check that it is the first time we see this switch or, if
               --  it is not the first time, the same path is specified.

               if RTS_Specified = null then
                  RTS_Specified := new String'(Argv (7 .. Argv'Last));

               elsif RTS_Specified.all /= Argv (7 .. Argv'Last) then
                  Osint.Fail ("--RTS cannot be specified multiple times");
               end if;

               --  Valid --RTS switch

               Opt.No_Stdinc := True;
               Opt.RTS_Switch := True;
            end if;

         else
            OK := False;
         end if;

      --  If not a switch, it must be a file name

      else
         Add_File (Argv);
      end if;

      if not OK then
         Write_Str ("warning: unknown switch """);
         Write_Str (Argv);
         Write_Line ("""");
      end if;

   end Scan_Ls_Arg;

   -----------
   -- Usage --
   -----------

   procedure Usage is
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

      Display_Usage_Version_And_Help;

      --  Line for -a

      Write_Str ("  -a         also output relevant predefined units");
      Write_Eol;

      --  Line for -u

      Write_Str ("  -u         output only relevant unit names");
      Write_Eol;

      --  Line for -h

      Write_Str ("  -h         output this help message");
      Write_Eol;

      --  Line for -s

      Write_Str ("  -s         output only relevant source names");
      Write_Eol;

      --  Line for -o

      Write_Str ("  -o         output only relevant object names");
      Write_Eol;

      --  Line for -d

      Write_Str ("  -d         output sources on which specified units " &
                               "depend");
      Write_Eol;

      --  Line for -l

      Write_Str ("  -l         output license information");
      Write_Eol;

      --  Line for -v

      Write_Str ("  -v         verbose output, full path and unit " &
                               "information");
      Write_Eol;
      Write_Eol;

      --  Line for -files=

      Write_Str ("  -files=fil files are listed in text file 'fil'");
      Write_Eol;

      --  Line for -aI switch

      Write_Str ("  -aIdir     specify source files search path");
      Write_Eol;

      --  Line for -aO switch

      Write_Str ("  -aOdir     specify object files search path");
      Write_Eol;

      --  Line for -aP switch

      Write_Str ("  -aPdir     specify project search path");
      Write_Eol;

      --  Line for -I switch

      Write_Str ("  -Idir      like -aIdir -aOdir");
      Write_Eol;

      --  Line for -I- switch

      Write_Str ("  -I-        do not look for sources & object files");
      Write_Str (" in the default directory");
      Write_Eol;

      --  Line for -nostdinc

      Write_Str ("  -nostdinc  do not look for source files");
      Write_Str (" in the system default directory");
      Write_Eol;

      --  Line for --RTS

      Write_Str ("  --RTS=dir  specify the default source and object search"
                 & " path");
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

   procedure Check_Version_And_Help is new Check_Version_And_Help_G (Usage);

--  Start of processing for Gnatls

begin
   --  Initialize standard packages

   Csets.Initialize;
   Snames.Initialize;

   --  First check for --version or --help

   Check_Version_And_Help ("GNATLS", "1997");

   --  Loop to scan out arguments

   Next_Arg := 1;
   Scan_Args : while Next_Arg < Arg_Count loop
      declare
         Next_Argv : String (1 .. Len_Arg (Next_Arg));
      begin
         Fill_Arg (Next_Argv'Address, Next_Arg);
         Scan_Ls_Arg (Next_Argv);
      end;

      Next_Arg := Next_Arg + 1;
   end loop Scan_Args;

   --  If -l (output license information) is given, it must be the only switch

   if License and then Arg_Count /= 2 then
      Set_Standard_Error;
      Write_Str ("Can't use -l with another switch");
      Write_Eol;
      Usage;
      Exit_Program (E_Fatal);
   end if;

   --  Handle --RTS switch

   if RTS_Specified /= null then
      Search_RTS (RTS_Specified.all);
   end if;

   --  Add the source and object directories specified on the command line, if
   --  any, to the searched directories.

   while First_Source_Dir /= null loop
      Add_Src_Search_Dir (First_Source_Dir.Value.all);
      First_Source_Dir := First_Source_Dir.Next;
   end loop;

   while First_Lib_Dir /= null loop
      Add_Lib_Search_Dir (First_Lib_Dir.Value.all);
      First_Lib_Dir := First_Lib_Dir.Next;
   end loop;

   --  Finally, add the default directories and obtain target parameters

   Osint.Add_Default_Search_Dirs;

   if Verbose_Mode then
      Write_Eol;
      Display_Version ("GNATLS", "1997");
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
      Write_Eol;
      Write_Str (Project_Search_Path);
      Write_Eol;
      Write_Str ("   <Current_Directory>");
      Write_Eol;

      Initialize_Default_Project_Path
        (Prj_Path, Target_Name => Sdefault.Target_Name.all);

      declare
         Project_Path : String_Access;
         First        : Natural;
         Last         : Natural;

      begin
         Get_Path (Prj_Path, Project_Path);

         if Project_Path.all /= "" then
            First := Project_Path'First;
            loop
               while First <= Project_Path'Last
                 and then (Project_Path (First) = Path_Separator)
               loop
                  First := First + 1;
               end loop;

               exit when First > Project_Path'Last;

               Last := First;
               while Last < Project_Path'Last
                 and then Project_Path (Last + 1) /= Path_Separator
               loop
                  Last := Last + 1;
               end loop;

               if First /= Last or else Project_Path (First) /= '.' then

                  --  If the directory is ".", skip it as it is the current
                  --  directory and it is already the first directory in the
                  --  project path.

                  Write_Str ("   ");
                  Write_Str
                    (Normalize_Pathname
                      (To_Host_Dir_Spec
                        (Project_Path (First .. Last), True).all));
                  Write_Eol;
               end if;

               First := Last + 1;
            end loop;
         end if;
      end;

      Write_Eol;
   end if;

   --  Output usage information when requested

   if Print_Usage then
      Usage;
   end if;

   --  Output license information when requested

   if License then
      Output_License_Information;
      Exit_Program (E_Success);
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
      Ali_File  := Full_Lib_File_Name (Lib_File_Name (Main_File));

      if Ali_File = No_File then
         if Very_Verbose_Mode then
            GNATDIST.Output_No_ALI (Lib_File_Name (Main_File));

         else
            Set_Standard_Error;
            Write_Str ("Can't find library info for ");
            Get_Name_String (Main_File);
            Write_Char ('"'); -- "
            Write_Str (Name_Buffer (1 .. Name_Len));
            Write_Char ('"'); -- "
            Write_Eol;
         end if;

      else
         Ali_File := Strip_Directory (Ali_File);

         if Get_Name_Table_Info (Ali_File) = 0 then
            Text := Read_Library_Info (Ali_File, True);

            declare
               Discard : ALI_Id;
               pragma Unreferenced (Discard);
            begin
               Discard :=
                 Scan_ALI
                   (Ali_File,
                    Text,
                    Ignore_ED     => False,
                    Err           => False,
                    Ignore_Errors => True);
            end;

            Free (Text);
         end if;
      end if;
   end loop;

   --  Reset default output file descriptor, if needed

   Set_Standard_Output;

   if Very_Verbose_Mode then
      for A in ALIs.First .. ALIs.Last loop
         GNATDIST.Output_ALI (A);
      end loop;

      return;
   end if;

   Find_General_Layout;

   for Id in ALIs.First .. ALIs.Last loop
      declare
         Last_U : Unit_Id;

      begin
         Get_Name_String (Units.Table (ALIs.Table (Id).First_Unit).Uname);

         if Also_Predef or else not Is_Internal_Unit then
            if ALIs.Table (Id).No_Object then
               Output_Object (No_File);
            else
               Output_Object (ALIs.Table (Id).Ofile_Full_Name);
            end if;

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

               Output_Unit (Id, U);

               --  Output source now, unless if it will be done as part of
               --  outputing dependencies.

               if not (Dependable and then Print_Source) then
                  Output_Source (Corresponding_Sdep_Entry (Id, U));
               end if;
            end loop;

            --  Print out list of units on which this unit depends (D lines)

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

   --  All done. Set proper exit status

   Namet.Finalize;
   Exit_Program (E_Success);
end Gnatls;
