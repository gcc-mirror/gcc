------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S I N P U T . L                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2004 Free Software Foundation, Inc.          --
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

with Alloc;
with Atree;    use Atree;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Namet;    use Namet;
with Opt;      use Opt;
with Osint;    use Osint;
with Output;   use Output;
with Prep;     use Prep;
with Prepcomp; use Prepcomp;
with Scans;    use Scans;
with Scn;      use Scn;
with Sinfo;    use Sinfo;
with System;   use System;

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
   --  "foreword", a single line indicationg what source the symbols apply to.
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
   --  Add an LF to Prep_Buffer.
   --  Used to initialize the preprocessor.

   function Load_File
     (N : File_Name_Type;
      T : Osint.File_Type) return Source_File_Index;
   --  Load a source file, a configuration pragmas file or a definition file
   --  Coding also allows preprocessing file, but not a library file ???

   -------------------------------
   -- Adjust_Instantiation_Sloc --
   -------------------------------

   procedure Adjust_Instantiation_Sloc (N : Node_Id; A : Sloc_Adjustment) is
      Loc : constant Source_Ptr := Sloc (N);

   begin
      --  We only do the adjustment if the value is between the appropriate
      --  low and high values. It is not clear that this should ever not be
      --  the case, but in practice there seem to be some nodes that get
      --  copied twice, and this is a defence against that happening.

      if A.Lo <= Loc and then Loc <= A.Hi then
         Set_Sloc (N, Loc + A.Adjust);
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
     (Inst_Node    : Entity_Id;
      Template_Id  : Entity_Id;
      Inlined_Body : Boolean;
      A            : out Sloc_Adjustment)
   is
      Dnod : constant Node_Id := Declaration_Node (Template_Id);
      Xold : Source_File_Index;
      Xnew : Source_File_Index;

   begin
      Xold := Get_Source_File_Index (Sloc (Template_Id));
      A.Lo := Source_File.Table (Xold).Source_First;
      A.Hi := Source_File.Table (Xold).Source_Last;

      Source_File.Increment_Last;
      Xnew := Source_File.Last;

      Source_File.Table (Xnew)               := Source_File.Table (Xold);
      Source_File.Table (Xnew).Inlined_Body  := Inlined_Body;
      Source_File.Table (Xnew).Instantiation := Sloc (Inst_Node);
      Source_File.Table (Xnew).Template      := Xold;

      --  Now we need to compute the new values of Source_First, Source_Last
      --  and adjust the source file pointer to have the correct virtual
      --  origin for the new range of values.

      Source_File.Table (Xnew).Source_First :=
        Source_File.Table (Xnew - 1).Source_Last + 1;
      A.Adjust := Source_File.Table (Xnew).Source_First - A.Lo;
      Source_File.Table (Xnew).Source_Last := A.Hi + A.Adjust;
      Set_Source_File_Index_Table (Xnew);

      Source_File.Table (Xnew).Sloc_Adjust :=
        Source_File.Table (Xold).Sloc_Adjust - A.Adjust;

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
         Write_Int (Int (A.Lo));
         Write_Eol;

         Write_Str ("  old hi = ");
         Write_Int (Int (A.Hi));
         Write_Eol;

         Write_Str ("  new lo = ");
         Write_Int (Int (Source_File.Table (Xnew).Source_First));
         Write_Eol;

         Write_Str ("  new hi = ");
         Write_Int (Int (Source_File.Table (Xnew).Source_Last));
         Write_Eol;

         Write_Str ("  adjustment factor = ");
         Write_Int (Int (A.Adjust));
         Write_Eol;

         Write_Str ("  instantiation location: ");
         Write_Location (Sloc (Inst_Node));
         Write_Eol;
      end if;

      --  For a given character in the source, a higher subscript will be
      --  used to access the instantiation, which means that the virtual
      --  origin must have a corresponding lower value. We compute this
      --  new origin by taking the address of the appropriate adjusted
      --  element in the old array. Since this adjusted element will be
      --  at a negative subscript, we must suppress checks.

      declare
         pragma Suppress (All_Checks);

         pragma Warnings (Off);
         --  This unchecked conversion is aliasing safe, since it is never
         --  used to create improperly aliased pointer values.

         function To_Source_Buffer_Ptr is new
           Unchecked_Conversion (Address, Source_Buffer_Ptr);

         pragma Warnings (On);

      begin
         Source_File.Table (Xnew).Source_Text :=
           To_Source_Buffer_Ptr
             (Source_File.Table (Xold).Source_Text (-A.Adjust)'Address);
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

      if T = Osint.Source then
         Prepare_To_Preprocess
           (Source => N, Preprocessing_Needed => Preprocessing_Needed);
      end if;

      Source_File.Increment_Last;
      X := Source_File.Last;

      if X = Source_File.First then
         Lo := First_Source_Ptr;
      else
         Lo := Source_File.Table (X - 1).Source_Last + 1;
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

               procedure Wchar (C : Character) is
               begin
                  if C < ' ' or C in ASCII.DEL .. Character'Val (16#9F#) then
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
                  Identifier_Casing   => Unknown,
                  Inlined_Body        => False,
                  Instantiation       => No_Location,
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
                  Time_Stamp          => Osint.Current_Source_File_Stamp);

            Alloc_Line_Tables (S, Opt.Table_Factor * Alloc.Lines_Initial);
            S.Lines_Table (1) := Lo;
         end;

         --  Preprocess the source if it needs to be preprocessed

         if Preprocessing_Needed then
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

            begin
               --  If this is the first time we preprocess a source, allocate
               --  the preprocessing buffer.

               if Prep_Buffer = null then
                  Prep_Buffer :=
                    new Text_Buffer (1 .. Initial_Size_Of_Prep_Buffer);
               end if;

               --  Make sure the preprocessing buffer is empty

               Prep_Buffer_Last := 0;

               --  Initialize the preprocessor

               Prep.Initialize
                 (Error_Msg         => Errout.Error_Msg'Access,
                  Scan              => Scn.Scanner.Scan'Access,
                  Set_Ignore_Errors => Errout.Set_Ignore_Errors'Access,
                  Put_Char          => Put_Char_In_Prep_Buffer'Access,
                  New_EOL           => New_EOL_In_Prep_Buffer'Access);

               --  Initialize the scanner and set its behavior for
               --  preprocessing, then preprocess.

               Scn.Scanner.Initialize_Scanner (No_Unit, X);

               Scn.Scanner.Set_Special_Character ('#');
               Scn.Scanner.Set_Special_Character ('$');
               Scn.Scanner.Set_End_Of_Line_As_Token (True);

               Preprocess;

               --  Reset the scanner to its standard behavior

               Scn.Scanner.Reset_Special_Characters;
               Scn.Scanner.Set_End_Of_Line_As_Token (False);

               --  If there were errors during preprocessing, record an
               --  error at the start of the file, and do not change the
               --  source buffer.

               if T /= Total_Errors_Detected then
                  Errout.Error_Msg
                    ("file could not be successfully preprocessed", Lo);
                  return No_Source_File;

               else
                  --  Set the new value of Hi

                  Hi := Lo + Source_Ptr (Prep_Buffer_Last);

                  --  Create the new source buffer

                  declare
                     subtype Actual_Source_Buffer is Source_Buffer (Lo .. Hi);
                     --  Physical buffer allocated

                     type Actual_Source_Ptr is access Actual_Source_Buffer;
                     --  This is the pointer type for the physical buffer
                     --  allocated.

                     Actual_Ptr : constant Actual_Source_Ptr :=
                                    new Actual_Source_Buffer;
                     --  And this is the actual physical buffer

                  begin
                     Actual_Ptr (Lo .. Hi - 1) :=
                       Prep_Buffer (1 .. Prep_Buffer_Last);
                     Actual_Ptr (Hi) := EOF;

                     --  Now we need to work out the proper virtual origin
                     --  pointer to return. This is exactly
                     --  Actual_Ptr (0)'Address, but we have to be careful to
                     --  suppress checks to compute this address.

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
                        --  have neccessarily the same starts and lengths.

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

   ----------------------------
   -- Source_File_Is_Subunit --
   ----------------------------

   function Source_File_Is_Subunit (X : Source_File_Index) return Boolean is
   begin
      Initialize_Scanner (No_Unit, X);

      --  We scan past junk to the first interesting compilation unit
      --  token, to see if it is SEPARATE. We ignore WITH keywords during
      --  this and also PRIVATE. The reason for ignoring PRIVATE is that
      --  it handles some error situations, and also it is possible that
      --  a PRIVATE WITH feature might be approved some time in the future.

      while Token = Tok_With
        or else Token = Tok_Private
        or else (Token not in Token_Class_Cunit and then Token /= Tok_EOF)
      loop
         Scan;
      end loop;

      return Token = Tok_Separate;
   end Source_File_Is_Subunit;

end Sinput.L;
