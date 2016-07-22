------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S I N P U T . L                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2016, Free Software Foundation, Inc.         --
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

with Alloc;
with Atree;    use Atree;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Fname;    use Fname;
with Lib;      use Lib;
with Opt;      use Opt;
with Osint;    use Osint;
with Output;   use Output;
with Prep;     use Prep;
with Prepcomp; use Prepcomp;
with Scans;    use Scans;
with Scn;      use Scn;
with Sem_Aux;  use Sem_Aux;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with System;   use System;

with System.OS_Lib; use System.OS_Lib;

with Unchecked_Conversion;

package body Sinput.L is

   Prep_Buffer : Text_Buffer_Ptr := null;
   --  A buffer to temporarily stored the result of preprocessing a source.
   --  It is only allocated if there is at least one source to preprocess.

   Prep_Buffer_Last : Text_Ptr := 0;
   --  Index of the last significant character in Prep_Buffer

   Initial_Size_Of_Prep_Buffer : constant := 10_000;
   --  Size of Prep_Buffer when it is first allocated

   --  When a file is to be preprocessed and the options to list symbols
   --  has been selected (switch -s), Prep.List_Symbols is called with a
   --  "foreword", a single line indicating what source the symbols apply to.
   --  The following two constant String are the start and the end of this
   --  foreword.

   Foreword_Start : constant String :=
                      "Preprocessing Symbols for source """;

   Foreword_End : constant String := """";

   -----------------
   -- Subprograms --
   -----------------

   procedure Put_Char_In_Prep_Buffer (C : Character);
   --  Add one character in Prep_Buffer, extending Prep_Buffer if need be.
   --  Used to initialize the preprocessor.

   procedure New_EOL_In_Prep_Buffer;
   --  Add an LF to Prep_Buffer (used to initialize the preprocessor)

   function Load_File
     (N : File_Name_Type;
      T : Osint.File_Type) return Source_File_Index;
   --  Load a source file, a configuration pragmas file or a definition file
   --  Coding also allows preprocessing file, but not a library file ???

   -------------------------------
   -- Adjust_Instantiation_Sloc --
   -------------------------------

   procedure Adjust_Instantiation_Sloc
     (N      : Node_Id;
      Factor : Sloc_Adjustment)
   is
      Loc : constant Source_Ptr := Sloc (N);

   begin
      --  We only do the adjustment if the value is between the appropriate low
      --  and high values. It is not clear that this should ever not be the
      --  case, but in practice there seem to be some nodes that get copied
      --  twice, and this is a defence against that happening.

      if Factor.Lo <= Loc and then Loc <= Factor.Hi then
         Set_Sloc (N, Loc + Factor.Adjust);
      end if;
   end Adjust_Instantiation_Sloc;

   --------------------------------
   -- Complete_Source_File_Entry --
   --------------------------------

   procedure Complete_Source_File_Entry is
      CSF : constant Source_File_Index := Current_Source_File;
   begin
      Trim_Lines_Table (CSF);
      Source_File.Table (CSF).Source_Checksum := Checksum;
   end Complete_Source_File_Entry;

   ---------------------------------
   -- Create_Instantiation_Source --
   ---------------------------------

   procedure Create_Instantiation_Source
     (Inst_Node        : Entity_Id;
      Template_Id      : Entity_Id;
      Factor           : out Sloc_Adjustment;
      Inlined_Body     : Boolean := False;
      Inherited_Pragma : Boolean := False)
   is
      Dnod : constant Node_Id := Declaration_Node (Template_Id);
      Xold : Source_File_Index;
      Xnew : Source_File_Index;

   begin
      Xold      := Get_Source_File_Index (Sloc (Template_Id));
      Factor.Lo := Source_File.Table (Xold).Source_First;
      Factor.Hi := Source_File.Table (Xold).Source_Last;

      Source_File.Append (Source_File.Table (Xold));
      Xnew := Source_File.Last;

      declare
         Sold : Source_File_Record renames Source_File.Table (Xold);
         Snew : Source_File_Record renames Source_File.Table (Xnew);

         Inst_Spec : Node_Id;

      begin
         Snew.Inlined_Body     := Inlined_Body;
         Snew.Inherited_Pragma := Inherited_Pragma;
         Snew.Template         := Xold;

         --  For a genuine generic instantiation, assign new instance id. For
         --  inlined bodies or inherited pragmas, we retain that of the
         --  template, but we save the call location.

         if Inlined_Body or Inherited_Pragma then
            Snew.Inlined_Call := Sloc (Inst_Node);

         else
            --  If the spec has been instantiated already, and we are now
            --  creating the instance source for the corresponding body now,
            --  retrieve the instance id that was assigned to the spec, which
            --  corresponds to the same instantiation sloc.

            Inst_Spec := Instance_Spec (Inst_Node);
            if Present (Inst_Spec) then
               declare
                  Inst_Spec_Ent : Entity_Id;
                  --  Instance spec entity

                  Inst_Spec_Sloc : Source_Ptr;
                  --  Virtual sloc of the spec instance source

                  Inst_Spec_Inst_Id : Instance_Id;
                  --  Instance id assigned to the instance spec

               begin
                  Inst_Spec_Ent := Defining_Entity (Inst_Spec);

                  --  For a subprogram instantiation, we want the subprogram
                  --  instance, not the wrapper package.

                  if Present (Related_Instance (Inst_Spec_Ent)) then
                     Inst_Spec_Ent := Related_Instance (Inst_Spec_Ent);
                  end if;

                  --  The specification of the instance entity has a virtual
                  --  sloc within the instance sloc range.

                  --  ??? But the Unit_Declaration_Node has the sloc of the
                  --  instantiation, which is somewhat of an oddity.

                  Inst_Spec_Sloc :=
                    Sloc
                      (Specification (Unit_Declaration_Node (Inst_Spec_Ent)));
                  Inst_Spec_Inst_Id :=
                    Source_File.Table
                      (Get_Source_File_Index (Inst_Spec_Sloc)).Instance;

                  pragma Assert
                    (Sloc (Inst_Node) = Instances.Table (Inst_Spec_Inst_Id));
                  Snew.Instance := Inst_Spec_Inst_Id;
               end;

            else
               Instances.Append (Sloc (Inst_Node));
               Snew.Instance := Instances.Last;
            end if;
         end if;

         --  Now compute the new values of Source_First and Source_Last and
         --  adjust the source file pointer to have the correct virtual origin
         --  for the new range of values.

         --  Source_First must be greater than the last Source_Last value and
         --  also must be a multiple of Source_Align.

         Snew.Source_First :=
           ((Source_File.Table (Xnew - 1).Source_Last + Source_Align) /
              Source_Align) * Source_Align;
         Factor.Adjust := Snew.Source_First - Factor.Lo;
         Snew.Source_Last := Factor.Hi + Factor.Adjust;

         Set_Source_File_Index_Table (Xnew);

         Snew.Sloc_Adjust := Sold.Sloc_Adjust - Factor.Adjust;

         if Debug_Flag_L then
            Write_Eol;
            Write_Str ("*** Create instantiation source for ");

            if Nkind (Dnod) in N_Proper_Body
              and then Was_Originally_Stub (Dnod)
            then
               Write_Str ("subunit ");

            elsif Ekind (Template_Id) = E_Generic_Package then
               if Nkind (Dnod) = N_Package_Body then
                  Write_Str ("body of package ");
               else
                  Write_Str ("spec of package ");
               end if;

            elsif Ekind (Template_Id) = E_Function then
               Write_Str ("body of function ");

            elsif Ekind (Template_Id) = E_Procedure then
               Write_Str ("body of procedure ");

            elsif Ekind (Template_Id) = E_Generic_Function then
               Write_Str ("spec of function ");

            elsif Ekind (Template_Id) = E_Generic_Procedure then
               Write_Str ("spec of procedure ");

            elsif Ekind (Template_Id) = E_Package_Body then
               Write_Str ("body of package ");

            else pragma Assert (Ekind (Template_Id) = E_Subprogram_Body);
               if Nkind (Dnod) = N_Procedure_Specification then
                  Write_Str ("body of procedure ");
               else
                  Write_Str ("body of function ");
               end if;
            end if;

            Write_Name (Chars (Template_Id));
            Write_Eol;

            Write_Str ("  new source index = ");
            Write_Int (Int (Xnew));
            Write_Eol;

            Write_Str ("  copying from file name = ");
            Write_Name (File_Name (Xold));
            Write_Eol;

            Write_Str ("  old source index = ");
            Write_Int (Int (Xold));
            Write_Eol;

            Write_Str ("  old lo = ");
            Write_Int (Int (Factor.Lo));
            Write_Eol;

            Write_Str ("  old hi = ");
            Write_Int (Int (Factor.Hi));
            Write_Eol;

            Write_Str ("  new lo = ");
            Write_Int (Int (Snew.Source_First));
            Write_Eol;

            Write_Str ("  new hi = ");
            Write_Int (Int (Snew.Source_Last));
            Write_Eol;

            Write_Str ("  adjustment factor = ");
            Write_Int (Int (Factor.Adjust));
            Write_Eol;

            Write_Str ("  instantiation location: ");
            Write_Location (Sloc (Inst_Node));
            Write_Eol;
         end if;

         --  For a given character in the source, a higher subscript will be
         --  used to access the instantiation, which means that the virtual
         --  origin must have a corresponding lower value. We compute this new
         --  origin by taking the address of the appropriate adjusted element
         --  in the old array. Since this adjusted element will be at a
         --  negative subscript, we must suppress checks.

         declare
            pragma Suppress (All_Checks);

            pragma Warnings (Off);
            --  This unchecked conversion is aliasing safe, since it is never
            --  used to create improperly aliased pointer values.

            function To_Source_Buffer_Ptr is new
              Unchecked_Conversion (Address, Source_Buffer_Ptr);

            pragma Warnings (On);

         begin
            Snew.Source_Text :=
              To_Source_Buffer_Ptr
                (Sold.Source_Text (-Factor.Adjust)'Address);
         end;
      end;
   end Create_Instantiation_Source;

   ----------------------
   -- Load_Config_File --
   ----------------------

   function Load_Config_File
     (N : File_Name_Type) return Source_File_Index
   is
   begin
      return Load_File (N, Osint.Config);
   end Load_Config_File;

   --------------------------
   -- Load_Definition_File --
   --------------------------

   function Load_Definition_File
     (N : File_Name_Type) return Source_File_Index
   is
   begin
      return Load_File (N, Osint.Definition);
   end Load_Definition_File;

   ---------------
   -- Load_File --
   ---------------

   function Load_File
     (N : File_Name_Type;
      T : Osint.File_Type) return Source_File_Index
   is
      Src : Source_Buffer_Ptr;
      X   : Source_File_Index;
      Lo  : Source_Ptr;
      Hi  : Source_Ptr;

      Preprocessing_Needed : Boolean := False;

   begin
      --  If already there, don't need to reload file. An exception occurs
      --  in multiple unit per file mode. It would be nice in this case to
      --  share the same source file for each unit, but this leads to many
      --  difficulties with assumptions (e.g. in the body of lib), that a
      --  unit can be found by locating its source file index. Since we do
      --  not expect much use of this mode, it's no big deal to waste a bit
      --  of space and time by reading and storing the source multiple times.

      if Multiple_Unit_Index = 0 then
         for J in 1 .. Source_File.Last loop
            if Source_File.Table (J).File_Name = N then
               return J;
            end if;
         end loop;
      end if;

      --  Here we must build a new entry in the file table

      --  But first, we must check if a source needs to be preprocessed,
      --  because we may have to load and parse a definition file, and we want
      --  to do that before we load the source, so that the buffer of the
      --  source will be the last created, and we will be able to replace it
      --  and modify Hi without stepping on another buffer.

      if T = Osint.Source and then not Is_Internal_File_Name (N) then
         Prepare_To_Preprocess
           (Source => N, Preprocessing_Needed => Preprocessing_Needed);
      end if;

      Source_File.Increment_Last;
      X := Source_File.Last;

      --  Compute starting index, respecting alignment requirement

      if X = Source_File.First then
         Lo := First_Source_Ptr;
      else
         Lo := ((Source_File.Table (X - 1).Source_Last + Source_Align) /
                  Source_Align) * Source_Align;
      end if;

      Osint.Read_Source_File (N, Lo, Hi, Src, T);

      if Src = null then
         Source_File.Decrement_Last;
         return No_Source_File;

      else
         if Debug_Flag_L then
            Write_Eol;
            Write_Str ("*** Build source file table entry, Index = ");
            Write_Int (Int (X));
            Write_Str (", file name = ");
            Write_Name (N);
            Write_Eol;
            Write_Str ("  lo = ");
            Write_Int (Int (Lo));
            Write_Eol;
            Write_Str ("  hi = ");
            Write_Int (Int (Hi));
            Write_Eol;

            Write_Str ("  first 10 chars -->");

            declare
               procedure Wchar (C : Character);
               --  Writes character or ? for control character

               -----------
               -- Wchar --
               -----------

               procedure Wchar (C : Character) is
               begin
                  if C < ' '
                    or else C in ASCII.DEL .. Character'Val (16#9F#)
                  then
                     Write_Char ('?');
                  else
                     Write_Char (C);
                  end if;
               end Wchar;

            begin
               for J in Lo .. Lo + 9 loop
                  Wchar (Src (J));
               end loop;

               Write_Str ("<--");
               Write_Eol;

               Write_Str ("  last 10 chars  -->");

               for J in Hi - 10 .. Hi - 1 loop
                  Wchar (Src (J));
               end loop;

               Write_Str ("<--");
               Write_Eol;

               if Src (Hi) /= EOF then
                  Write_Str ("  error: no EOF at end");
                  Write_Eol;
               end if;
            end;
         end if;

         declare
            S         : Source_File_Record renames Source_File.Table (X);
            File_Type : Type_Of_File;

         begin
            case T is
               when Osint.Source =>
                  File_Type := Sinput.Src;

               when Osint.Library =>
                  raise Program_Error;

               when Osint.Config =>
                  File_Type := Sinput.Config;

               when Osint.Definition =>
                  File_Type := Def;

               when Osint.Preprocessing_Data =>
                  File_Type := Preproc;
            end case;

            S := (Debug_Source_Name   => N,
                  File_Name           => N,
                  File_Type           => File_Type,
                  First_Mapped_Line   => No_Line_Number,
                  Full_Debug_Name     => Osint.Full_Source_Name,
                  Full_File_Name      => Osint.Full_Source_Name,
                  Full_Ref_Name       => Osint.Full_Source_Name,
                  Instance            => No_Instance_Id,
                  Identifier_Casing   => Unknown,
                  Inlined_Call        => No_Location,
                  Inlined_Body        => False,
                  Inherited_Pragma    => False,
                  Keyword_Casing      => Unknown,
                  Last_Source_Line    => 1,
                  License             => Unknown,
                  Lines_Table         => null,
                  Lines_Table_Max     => 1,
                  Logical_Lines_Table => null,
                  Num_SRef_Pragmas    => 0,
                  Reference_Name      => N,
                  Sloc_Adjust         => 0,
                  Source_Checksum     => 0,
                  Source_First        => Lo,
                  Source_Last         => Hi,
                  Source_Text         => Src,
                  Template            => No_Source_File,
                  Unit                => No_Unit,
                  Time_Stamp          => Osint.Current_Source_File_Stamp);

            Alloc_Line_Tables (S, Opt.Table_Factor * Alloc.Lines_Initial);
            S.Lines_Table (1) := Lo;
         end;

         --  Preprocess the source if it needs to be preprocessed

         if Preprocessing_Needed then

            --  Temporarily set the Source_File_Index_Table entries for the
            --  source, to avoid crash when reporting an error.

            Set_Source_File_Index_Table (X);

            if Opt.List_Preprocessing_Symbols then
               Get_Name_String (N);

               declare
                  Foreword : String (1 .. Foreword_Start'Length +
                                          Name_Len + Foreword_End'Length);

               begin
                  Foreword (1 .. Foreword_Start'Length) := Foreword_Start;
                  Foreword (Foreword_Start'Length + 1 ..
                              Foreword_Start'Length + Name_Len) :=
                    Name_Buffer (1 .. Name_Len);
                  Foreword (Foreword'Last - Foreword_End'Length + 1 ..
                              Foreword'Last) := Foreword_End;
                  Prep.List_Symbols (Foreword);
               end;
            end if;

            declare
               T : constant Nat := Total_Errors_Detected;
               --  Used to check if there were errors during preprocessing

               Save_Style_Check : Boolean;
               --  Saved state of the Style_Check flag (which needs to be
               --  temporarily set to False during preprocessing, see below).

               Modified : Boolean;

            begin
               --  If this is the first time we preprocess a source, allocate
               --  the preprocessing buffer.

               if Prep_Buffer = null then
                  Prep_Buffer :=
                    new Text_Buffer (1 .. Initial_Size_Of_Prep_Buffer);
               end if;

               --  Make sure the preprocessing buffer is empty

               Prep_Buffer_Last := 0;

               --  Initialize the preprocessor hooks

               Prep.Setup_Hooks
                 (Error_Msg         => Errout.Error_Msg'Access,
                  Scan              => Scn.Scanner.Scan'Access,
                  Set_Ignore_Errors => Errout.Set_Ignore_Errors'Access,
                  Put_Char          => Put_Char_In_Prep_Buffer'Access,
                  New_EOL           => New_EOL_In_Prep_Buffer'Access);

               --  Initialize scanner and set its behavior for preprocessing,
               --  then preprocess. Also disable style checks, since some of
               --  them are done in the scanner (specifically, those dealing
               --  with line length and line termination), and cannot be done
               --  during preprocessing (because the source file index table
               --  has not been set yet).

               Scn.Scanner.Initialize_Scanner (X);

               Scn.Scanner.Set_Special_Character ('#');
               Scn.Scanner.Set_Special_Character ('$');
               Scn.Scanner.Set_End_Of_Line_As_Token (True);
               Save_Style_Check := Opt.Style_Check;
               Opt.Style_Check := False;

               --  The actual preprocessing step

               Preprocess (Modified);

               --  Reset the scanner to its standard behavior, and restore the
               --  Style_Checks flag.

               Scn.Scanner.Reset_Special_Characters;
               Scn.Scanner.Set_End_Of_Line_As_Token (False);
               Opt.Style_Check := Save_Style_Check;

               --  If there were errors during preprocessing, record an error
               --  at the start of the file, and do not change the source
               --  buffer.

               if T /= Total_Errors_Detected then
                  Errout.Error_Msg
                    ("file could not be successfully preprocessed", Lo);
                  return No_Source_File;

               else
                  --  Output the result of the preprocessing, if requested and
                  --  the source has been modified by the preprocessing. Only
                  --  do that for the main unit (spec, body and subunits).

                  if Generate_Processed_File
                    and then Modified
                    and then
                     ((Compiler_State = Parsing
                        and then Parsing_Main_Extended_Source)
                       or else
                        (Compiler_State = Analyzing
                          and then Analysing_Subunit_Of_Main))
                  then
                     declare
                        FD     : File_Descriptor;
                        NB     : Integer;
                        Status : Boolean;

                     begin
                        Get_Name_String (N);
                        Add_Str_To_Name_Buffer (Prep_Suffix);

                        Delete_File (Name_Buffer (1 .. Name_Len), Status);

                        FD :=
                          Create_New_File (Name_Buffer (1 .. Name_Len), Text);

                        Status := FD /= Invalid_FD;

                        if Status then
                           NB :=
                             Write
                               (FD,
                                Prep_Buffer (1)'Address,
                                Integer (Prep_Buffer_Last));
                           Status := NB = Integer (Prep_Buffer_Last);
                        end if;

                        if Status then
                           Close (FD, Status);
                        end if;

                        if not Status then
                           Errout.Error_Msg
                             ("??could not write processed file """ &
                              Name_Buffer (1 .. Name_Len) & '"',
                              Lo);
                        end if;
                     end;
                  end if;

                  --  Set the new value of Hi

                  Hi := Lo + Source_Ptr (Prep_Buffer_Last);

                  --  Create the new source buffer

                  declare
                     subtype Actual_Source_Buffer is Source_Buffer (Lo .. Hi);
                     --  Physical buffer allocated

                     type Actual_Source_Ptr is access Actual_Source_Buffer;
                     --  Pointer type for the physical buffer allocated

                     Actual_Ptr : constant Actual_Source_Ptr :=
                                    new Actual_Source_Buffer;
                     --  Actual physical buffer

                  begin
                     Actual_Ptr (Lo .. Hi - 1) :=
                       Prep_Buffer (1 .. Prep_Buffer_Last);
                     Actual_Ptr (Hi) := EOF;

                     --  Now we need to work out the proper virtual origin
                     --  pointer to return. This is Actual_Ptr (0)'Address, but
                     --  we have to be careful to suppress checks to compute
                     --  this address.

                     declare
                        pragma Suppress (All_Checks);

                        pragma Warnings (Off);
                        --  This unchecked conversion is aliasing safe, since
                        --  it is never used to create improperly aliased
                        --  pointer values.

                        function To_Source_Buffer_Ptr is new
                          Unchecked_Conversion (Address, Source_Buffer_Ptr);

                        pragma Warnings (On);

                     begin
                        Src := To_Source_Buffer_Ptr (Actual_Ptr (0)'Address);

                        --  Record in the table the new source buffer and the
                        --  new value of Hi.

                        Source_File.Table (X).Source_Text := Src;
                        Source_File.Table (X).Source_Last := Hi;

                        --  Reset Last_Line to 1, because the lines do not
                        --  have necessarily the same starts and lengths.

                        Source_File.Table (X).Last_Source_Line := 1;
                     end;
                  end;
               end if;
            end;
         end if;

         Set_Source_File_Index_Table (X);
         return X;
      end if;
   end Load_File;

   ----------------------------------
   -- Load_Preprocessing_Data_File --
   ----------------------------------

   function Load_Preprocessing_Data_File
     (N : File_Name_Type) return Source_File_Index
   is
   begin
      return Load_File (N, Osint.Preprocessing_Data);
   end Load_Preprocessing_Data_File;

   ----------------------
   -- Load_Source_File --
   ----------------------

   function Load_Source_File
     (N : File_Name_Type) return Source_File_Index
   is
   begin
      return Load_File (N, Osint.Source);
   end Load_Source_File;

   ----------------------------
   -- New_EOL_In_Prep_Buffer --
   ----------------------------

   procedure New_EOL_In_Prep_Buffer is
   begin
      Put_Char_In_Prep_Buffer (ASCII.LF);
   end New_EOL_In_Prep_Buffer;

   -----------------------------
   -- Put_Char_In_Prep_Buffer --
   -----------------------------

   procedure Put_Char_In_Prep_Buffer (C : Character) is
   begin
      --  If preprocessing buffer is not large enough, double it

      if Prep_Buffer_Last = Prep_Buffer'Last then
         declare
            New_Prep_Buffer : constant Text_Buffer_Ptr :=
              new Text_Buffer (1 .. 2 * Prep_Buffer_Last);

         begin
            New_Prep_Buffer (Prep_Buffer'Range) := Prep_Buffer.all;
            Free (Prep_Buffer);
            Prep_Buffer := New_Prep_Buffer;
         end;
      end if;

      Prep_Buffer_Last := Prep_Buffer_Last + 1;
      Prep_Buffer (Prep_Buffer_Last) := C;
   end Put_Char_In_Prep_Buffer;

   -------------------------
   -- Source_File_Is_Body --
   -------------------------

   function Source_File_Is_Body (X : Source_File_Index) return Boolean is
      Pcount : Natural;

   begin
      Initialize_Scanner (No_Unit, X);

      --  Loop to look for subprogram or package body

      loop
         case Token is

            --  PRAGMA, WITH, USE (which can appear before a body)

            when Tok_Pragma | Tok_With | Tok_Use =>

               --  We just want to skip any of these, do it by skipping to a
               --  semicolon, but check for EOF, in case we have bad syntax.

               loop
                  if Token = Tok_Semicolon then
                     Scan;
                     exit;
                  elsif Token = Tok_EOF then
                     return False;
                  else
                     Scan;
                  end if;
               end loop;

            --  PACKAGE

            when Tok_Package =>
               Scan; -- Past PACKAGE

               --  We have a body if and only if BODY follows

               return Token = Tok_Body;

            --  FUNCTION or PROCEDURE

            when Tok_Procedure | Tok_Function =>
               Pcount := 0;

               --  Loop through tokens following PROCEDURE or FUNCTION

               loop
                  Scan;

                  case Token is

                     --  For parens, count paren level (note that paren level
                     --  can get greater than 1 if we have default parameters).

                     when Tok_Left_Paren =>
                        Pcount := Pcount + 1;

                     when Tok_Right_Paren =>
                        Pcount := Pcount - 1;

                     --  EOF means something weird, probably no body

                     when Tok_EOF =>
                        return False;

                     --  BEGIN or IS or END definitely means body is present

                     when Tok_Begin | Tok_Is | Tok_End =>
                        return True;

                     --  Semicolon means no body present if at outside any
                     --  parens. If within parens, ignore, since it could be
                     --  a parameter separator.

                     when Tok_Semicolon =>
                        if Pcount = 0 then
                           return False;
                        end if;

                     --  Skip anything else

                     when others =>
                        null;
                  end case;
               end loop;

            --  Anything else in main scan means we don't have a body

            when others =>
               return False;
         end case;
      end loop;
   end Source_File_Is_Body;

   ----------------------------
   -- Source_File_Is_No_Body --
   ----------------------------

   function Source_File_Is_No_Body (X : Source_File_Index) return Boolean is
   begin
      Initialize_Scanner (No_Unit, X);

      if Token /= Tok_Pragma then
         return False;
      end if;

      Scan; -- past pragma

      if Token /= Tok_Identifier
        or else Chars (Token_Node) /= Name_No_Body
      then
         return False;
      end if;

      Scan; -- past No_Body

      if Token /= Tok_Semicolon then
         return False;
      end if;

      Scan; -- past semicolon

      return Token = Tok_EOF;
   end Source_File_Is_No_Body;

end Sinput.L;
