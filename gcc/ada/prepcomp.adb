------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P R E P C O M P                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2003-2020, Free Software Foundation, Inc.         --
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

with Atree;    use Atree;
with Errout;   use Errout;
with Lib.Writ; use Lib.Writ;
with Opt;      use Opt;
with Osint;    use Osint;
with Prep;     use Prep;
with Scans;    use Scans;
with Scn;      use Scn;
with Sinput.L; use Sinput.L;
with Stringt;  use Stringt;
with Table;

package body Prepcomp is

   No_Preprocessing : Boolean := True;
   --  Set to False if there is at least one source that needs to be
   --  preprocessed.

   Source_Index_Of_Preproc_Data_File : Source_File_Index := No_Source_File;

   --  The following variable should be a constant, but this is not possible
   --  because its type GNAT.Dynamic_Tables.Instance has a component P of
   --  uninitialized private type GNAT.Dynamic_Tables.Table_Private and there
   --  are no exported values for this private type. Warnings are Off because
   --  it is never assigned a value.

   pragma Warnings (Off);
   No_Mapping : Prep.Symbol_Table.Instance;
   pragma Warnings (On);

   type Preproc_Data is record
      Mapping      : Symbol_Table.Instance;
      File_Name    : File_Name_Type := No_File;
      Deffile      : String_Id      := No_String;
      Undef_False  : Boolean        := False;
      Always_Blank : Boolean        := False;
      Comments     : Boolean        := False;
      No_Deletion  : Boolean        := False;
      List_Symbols : Boolean        := False;
      Processed    : Boolean        := False;
   end record;
   --  Structure to keep the preprocessing data for a file name or for the
   --  default (when Name_Id = No_Name).

   No_Preproc_Data : constant Preproc_Data :=
     (Mapping      => No_Mapping,
      File_Name    => No_File,
      Deffile      => No_String,
      Undef_False  => False,
      Always_Blank => False,
      Comments     => False,
      No_Deletion  => False,
      List_Symbols => False,
      Processed    => False);

   Default_Data : Preproc_Data := No_Preproc_Data;
   --  The preprocessing data to be used when no specific preprocessing data
   --  is specified for a source.

   Default_Data_Defined : Boolean := False;
   --  True if source for which no specific preprocessing is specified need to
   --  be preprocess with the Default_Data.

   Current_Data : Preproc_Data := No_Preproc_Data;

   package Preproc_Data_Table is new Table.Table
     (Table_Component_Type => Preproc_Data,
      Table_Index_Type     => Int,
      Table_Low_Bound      => 1,
      Table_Initial        => 5,
      Table_Increment      => 100,
      Table_Name           => "Prepcomp.Preproc_Data_Table");
   --  Table to store the specific preprocessing data

   Command_Line_Symbols : Symbol_Table.Instance;
   --  A table to store symbol definitions specified on the command line with
   --  -gnateD switches.

   package Dependencies is new Table.Table
     (Table_Component_Type => Source_File_Index,
      Table_Index_Type     => Int,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 100,
      Table_Name           => "Prepcomp.Dependencies");
   --  Table to store the dependencies on preprocessing files

   procedure Add_Command_Line_Symbols;
   --  Add the command line symbol definitions, if any, to Prep.Mapping table

   procedure Skip_To_End_Of_Line;
   --  Ignore errors and scan up to the next end of line or the end of file

   ------------------------------
   -- Add_Command_Line_Symbols --
   ------------------------------

   procedure Add_Command_Line_Symbols is
      Symbol_Id : Prep.Symbol_Id;

   begin
      for J in 1 .. Symbol_Table.Last (Command_Line_Symbols) loop
         Symbol_Id := Prep.Index_Of (Command_Line_Symbols.Table (J).Symbol);

         if Symbol_Id = No_Symbol then
            Symbol_Table.Increment_Last (Prep.Mapping);
            Symbol_Id := Symbol_Table.Last (Prep.Mapping);
         end if;

         Prep.Mapping.Table (Symbol_Id) := Command_Line_Symbols.Table (J);
      end loop;
   end Add_Command_Line_Symbols;

   --------------------
   -- Add_Dependency --
   --------------------

   procedure Add_Dependency (S : Source_File_Index) is
   begin
      Dependencies.Increment_Last;
      Dependencies.Table (Dependencies.Last) := S;
   end Add_Dependency;

   ----------------------
   -- Add_Dependencies --
   ----------------------

   procedure Add_Dependencies is
   begin
      for Index in 1 .. Dependencies.Last loop
         Add_Preprocessing_Dependency (Dependencies.Table (Index));
      end loop;
   end Add_Dependencies;

   -------------------
   -- Check_Symbols --
   -------------------

   procedure Check_Symbols is
   begin
      --  If there is at least one switch -gnateD specified

      if Symbol_Table.Last (Command_Line_Symbols) >= 1 then
         Current_Data := No_Preproc_Data;
         No_Preprocessing := False;
         Current_Data.Processed := True;

         --  Start with an empty, initialized mapping table; use Prep.Mapping,
         --  because Prep.Index_Of uses Prep.Mapping.

         Prep.Mapping := No_Mapping;
         Symbol_Table.Init (Prep.Mapping);

         --  Add the command line symbols

         Add_Command_Line_Symbols;

         --  Put the resulting Prep.Mapping in Current_Data, and immediately
         --  set Prep.Mapping to nil.

         Current_Data.Mapping := Prep.Mapping;
         Prep.Mapping := No_Mapping;

         --  Set the default data

         Default_Data := Current_Data;
         Default_Data_Defined := True;
      end if;
   end Check_Symbols;

   -----------------------------------
   -- Parse_Preprocessing_Data_File --
   -----------------------------------

   procedure Parse_Preprocessing_Data_File (N : File_Name_Type) is
      OK            : Boolean := False;
      Dash_Location : Source_Ptr;
      Symbol_Data   : Prep.Symbol_Data;
      Symbol_Id     : Prep.Symbol_Id;
      T             : constant Nat := Total_Errors_Detected;

   begin
      --  Load the preprocessing data file

      Source_Index_Of_Preproc_Data_File := Load_Preprocessing_Data_File (N);

      --  Fail if preprocessing data file cannot be found

      if Source_Index_Of_Preproc_Data_File = No_Source_File then
         Get_Name_String (N);
         Fail ("preprocessing data file """
               & Name_Buffer (1 .. Name_Len)
               & """ not found");
      end if;

      --  Initialize scanner and set its behavior for processing a data file

      Scn.Scanner.Initialize_Scanner (Source_Index_Of_Preproc_Data_File);
      Scn.Scanner.Set_End_Of_Line_As_Token (True);
      Scn.Scanner.Reset_Special_Characters;

      For_Each_Line : loop
         <<Scan_Line>>
         Scan;

         exit For_Each_Line when Token = Tok_EOF;

         if Token = Tok_End_Of_Line then
            goto Scan_Line;
         end if;

         --  Line is not empty

         OK := False;
         No_Preprocessing := False;
         Current_Data := No_Preproc_Data;

         case Token is
            when Tok_Asterisk =>

               --  Default data

               if Default_Data_Defined then
                  Error_Msg
                    ("multiple default preprocessing data", Token_Ptr);

               else
                  OK := True;
                  Default_Data_Defined := True;
               end if;

            when Tok_String_Literal =>

               --  Specific data

               String_To_Name_Buffer (String_Literal_Id);
               Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
               Current_Data.File_Name := Name_Find;
               OK := True;

               for Index in 1 .. Preproc_Data_Table.Last loop
                  if Current_Data.File_Name =
                       Preproc_Data_Table.Table (Index).File_Name
                  then
                     Error_Msg_File_1 := Current_Data.File_Name;
                     Error_Msg
                       ("multiple preprocessing data for{", Token_Ptr);
                     OK := False;
                     exit;
                  end if;
               end loop;

            when others =>
               Error_Msg ("`'*` or literal string expected", Token_Ptr);
         end case;

         --  If there is a problem, skip the line

         if not OK then
            Skip_To_End_Of_Line;
            goto Scan_Line;
         end if;

         --  Scan past the * or the literal string

         Scan;

         --  A literal string in second position is a definition file

         if Token = Tok_String_Literal then
            Current_Data.Deffile := String_Literal_Id;
            Current_Data.Processed := False;
            Scan;

         else
            --  If there is no definition file, set Processed to True now

            Current_Data.Processed := True;
         end if;

         --  Start with an empty, initialized mapping table; use Prep.Mapping,
         --  because Prep.Index_Of uses Prep.Mapping.

         Prep.Mapping := No_Mapping;
         Symbol_Table.Init (Prep.Mapping);

         --  Check the switches that may follow

         while Token /= Tok_End_Of_Line and then Token /= Tok_EOF loop
            if Token /= Tok_Minus then
               Error_Msg -- CODEFIX
                 ("`'-` expected", Token_Ptr);
               Skip_To_End_Of_Line;
               goto Scan_Line;
            end if;

            --  Keep the location of the '-' for possible error reporting

            Dash_Location := Token_Ptr;

            --  Scan past the '-'

            Scan;
            OK := False;
            Change_Reserved_Keyword_To_Symbol;

            --  An identifier (or a reserved word converted to an
            --  identifier) is expected and there must be no blank space
            --  between the '-' and the identifier.

            if Token = Tok_Identifier
              and then Token_Ptr = Dash_Location + 1
            then
               Get_Name_String (Token_Name);

               --  Check the character in the source, because the case is
               --  significant.

               case Sinput.Source (Token_Ptr) is
                  when 'a' =>

                     --  All source text preserved (also implies -u)

                     if Name_Len = 1 then
                        Current_Data.No_Deletion := True;
                        Current_Data.Undef_False := True;
                        OK := True;
                     end if;

                  when 'u' =>

                     --  Undefined symbol are False

                     if Name_Len = 1 then
                        Current_Data.Undef_False := True;
                        OK := True;
                     end if;

                  when 'b' =>

                     --  Blank lines

                     if Name_Len = 1 then
                        Current_Data.Always_Blank := True;
                        OK := True;
                     end if;

                  when 'c' =>

                     --  Comment removed lines

                     if Name_Len = 1 then
                        Current_Data.Comments := True;
                        OK := True;
                     end if;

                  when 's' =>

                     --  List symbols

                     if Name_Len = 1 then
                        Current_Data.List_Symbols := True;
                        OK := True;
                     end if;

                  when 'D' =>

                     --  Symbol definition

                     OK := Name_Len > 1;

                     if OK then

                        --  A symbol must be an Ada identifier; it cannot start
                        --  with an underline or a digit.

                        if Name_Buffer (2) = '_'
                          or else Name_Buffer (2) in '0' .. '9'
                        then
                           Error_Msg ("symbol expected", Token_Ptr + 1);
                           Skip_To_End_Of_Line;
                           goto Scan_Line;
                        end if;

                        --  Get the name id of the symbol

                        Symbol_Data.On_The_Command_Line := True;
                        Name_Buffer (1 .. Name_Len - 1) :=
                          Name_Buffer (2 .. Name_Len);
                        Name_Len := Name_Len - 1;
                        Symbol_Data.Symbol := Name_Find;

                        if Name_Buffer (1 .. Name_Len) = "if"
                          or else Name_Buffer (1 .. Name_Len) = "else"
                          or else Name_Buffer (1 .. Name_Len) = "elsif"
                          or else Name_Buffer (1 .. Name_Len) = "end"
                          or else Name_Buffer (1 .. Name_Len) = "not"
                          or else Name_Buffer (1 .. Name_Len) = "and"
                          or else Name_Buffer (1 .. Name_Len) = "then"
                        then
                           Error_Msg ("symbol expected", Token_Ptr + 1);
                           Skip_To_End_Of_Line;
                           goto Scan_Line;
                        end if;

                        --  Get the name id of the original symbol, with
                        --  possibly capital letters.

                        Name_Len := Integer (Scan_Ptr - Token_Ptr - 1);

                        for J in 1 .. Name_Len loop
                           Name_Buffer (J) :=
                             Sinput.Source (Token_Ptr + Text_Ptr (J));
                        end loop;

                        Symbol_Data.Original := Name_Find;

                        --  Scan past D<symbol>

                        Scan;

                        if Token /= Tok_Equal then
                           Error_Msg -- CODEFIX
                             ("`=` expected", Token_Ptr);
                           Skip_To_End_Of_Line;
                           goto Scan_Line;
                        end if;

                        --  Scan past '='

                        Scan;

                        --  Here any reserved word is OK

                        Change_Reserved_Keyword_To_Symbol
                          (All_Keywords => True);

                        --  Value can be an identifier (or a reserved word)
                        --  or a literal string.

                        case Token is
                           when Tok_String_Literal =>
                              Symbol_Data.Is_A_String := True;
                              Symbol_Data.Value := String_Literal_Id;

                           when Tok_Identifier =>
                              Symbol_Data.Is_A_String := False;
                              Start_String;

                              for J in Token_Ptr .. Scan_Ptr - 1 loop
                                 Store_String_Char (Sinput.Source (J));
                              end loop;

                              Symbol_Data.Value := End_String;

                           when others =>
                              Error_Msg
                                ("literal string or identifier expected",
                                 Token_Ptr);
                              Skip_To_End_Of_Line;
                              goto Scan_Line;
                        end case;

                        --  If symbol already exists, replace old definition
                        --  by new one.

                        Symbol_Id := Prep.Index_Of (Symbol_Data.Symbol);

                        --  Otherwise, add a new entry in the table

                        if Symbol_Id = No_Symbol then
                           Symbol_Table.Increment_Last (Prep.Mapping);
                           Symbol_Id := Symbol_Table.Last (Mapping);
                        end if;

                        Prep.Mapping.Table (Symbol_Id) := Symbol_Data;
                     end if;

                  when others =>
                     null;
               end case;

               Scan;
            end if;

            if not OK then
               Error_Msg ("invalid switch", Dash_Location);
               Skip_To_End_Of_Line;
               goto Scan_Line;
            end if;
         end loop;

         --  Add the command line symbols, if any, possibly replacing symbols
         --  just defined.

         Add_Command_Line_Symbols;

         --  Put the resulting Prep.Mapping in Current_Data, and immediately
         --  set Prep.Mapping to nil.

         Current_Data.Mapping := Prep.Mapping;
         Prep.Mapping := No_Mapping;

         --  Record Current_Data

         if Current_Data.File_Name = No_File then
            Default_Data := Current_Data;

         else
            Preproc_Data_Table.Increment_Last;
            Preproc_Data_Table.Table (Preproc_Data_Table.Last) := Current_Data;
         end if;

         Current_Data := No_Preproc_Data;
      end loop For_Each_Line;

      Scn.Scanner.Set_End_Of_Line_As_Token (False);

      --  Fail if there were errors in the preprocessing data file

      if Total_Errors_Detected > T then
         Errout.Finalize (Last_Call => True);
         Errout.Output_Messages;
         Fail ("errors found in preprocessing data file """
               & Get_Name_String (N) & """");
      end if;

      --  Record the dependency on the preprocessor data file

      Add_Dependency (Source_Index_Of_Preproc_Data_File);
   end Parse_Preprocessing_Data_File;

   ---------------------------
   -- Prepare_To_Preprocess --
   ---------------------------

   procedure Prepare_To_Preprocess
     (Source               : File_Name_Type;
      Preprocessing_Needed : out Boolean)
   is
      Default : Boolean := False;
      Index   : Int := 0;

   begin
      --  By default, preprocessing is not needed

      Preprocessing_Needed := False;

      if No_Preprocessing then
         return;
      end if;

      --  First, look for preprocessing data specific to the current source

      for J in 1 .. Preproc_Data_Table.Last loop
         if Preproc_Data_Table.Table (J).File_Name = Source then
            Index := J;
            Current_Data := Preproc_Data_Table.Table (J);
            exit;
         end if;
      end loop;

      --  If no specific preprocessing data, then take the default

      if Index = 0 then
         if Default_Data_Defined then
            Current_Data := Default_Data;
            Default := True;

         else
            --  If no default, then nothing to do

            return;
         end if;
      end if;

      --  Set the preprocessing flags according to the preprocessing data

      if Current_Data.Comments and not Current_Data.Always_Blank then
         Comment_Deleted_Lines := True;
         Blank_Deleted_Lines   := False;
      else
         Comment_Deleted_Lines := False;
         Blank_Deleted_Lines   := True;
      end if;

      No_Deletion                 := Current_Data.No_Deletion;
      Undefined_Symbols_Are_False := Current_Data.Undef_False;
      List_Preprocessing_Symbols  := Current_Data.List_Symbols;

      --  If not already done it, process the definition file

      if Current_Data.Processed then

         --  Set Prep.Mapping

         Prep.Mapping := Current_Data.Mapping;

      else
         --  First put the mapping in Prep.Mapping, because Prep.Parse_Def_File
         --  works on Prep.Mapping.

         Prep.Mapping := Current_Data.Mapping;

         String_To_Name_Buffer (Current_Data.Deffile);

         declare
            N       : constant File_Name_Type    := Name_Find;
            Deffile : constant Source_File_Index := Load_Definition_File (N);
            T       : constant Nat               := Total_Errors_Detected;

            Add_Deffile : Boolean := True;

         begin
            if Deffile <= No_Source_File then
               Fail
                 ("definition file """ & Get_Name_String (N) & """ not found");
            end if;

            --  Initialize the preprocessor and set the characteristics of the
            --  scanner for a definition file.

            Prep.Setup_Hooks
              (Error_Msg         => Errout.Error_Msg'Access,
               Scan              => Scn.Scanner.Scan'Access,
               Set_Ignore_Errors => Errout.Set_Ignore_Errors'Access,
               Put_Char          => null,
               New_EOL           => null);

            Scn.Scanner.Set_End_Of_Line_As_Token (True);
            Scn.Scanner.Reset_Special_Characters;

            --  Initialize the scanner and process the definition file

            Scn.Scanner.Initialize_Scanner (Deffile);
            Prep.Parse_Def_File;

            --  Reset the behavior of the scanner to the default

            Scn.Scanner.Set_End_Of_Line_As_Token (False);

            --  Fail if errors were found while processing the definition file

            if T /= Total_Errors_Detected then
               Errout.Finalize (Last_Call => True);
               Errout.Output_Messages;
               Fail ("errors found in definition file """
                     & Get_Name_String (N)
                     & """");
            end if;

            for Index in 1 .. Dependencies.Last loop
               if Dependencies.Table (Index) = Deffile then
                  Add_Deffile := False;
                  exit;
               end if;
            end loop;

            if Add_Deffile then
               Add_Dependency (Deffile);
            end if;
         end;

         --  Get back the mapping, indicate that the definition file is
         --  processed and store back the preprocessing data.

         Current_Data.Mapping := Prep.Mapping;
         Current_Data.Processed := True;

         if Default then
            Default_Data := Current_Data;

         else
            Preproc_Data_Table.Table (Index) := Current_Data;
         end if;
      end if;

      Preprocessing_Needed := True;
   end Prepare_To_Preprocess;

   ---------------------------------------------
   -- Process_Command_Line_Symbol_Definitions --
   ---------------------------------------------

   procedure Process_Command_Line_Symbol_Definitions is
      Symbol_Data : Prep.Symbol_Data;
      Found : Boolean := False;

   begin
      Symbol_Table.Init (Command_Line_Symbols);

      --  The command line definitions have been stored temporarily in
      --  array Symbol_Definitions.

      for Index in 1 .. Preprocessing_Symbol_Last loop
         --  Check each symbol definition, fail immediately if syntax is not
         --  correct.

         Check_Command_Line_Symbol_Definition
           (Definition => Preprocessing_Symbol_Defs (Index).all,
            Data       => Symbol_Data);
         Found := False;

         --  If there is already a definition for this symbol, replace the old
         --  definition by this one.

         for J in 1 .. Symbol_Table.Last (Command_Line_Symbols) loop
            if Command_Line_Symbols.Table (J).Symbol = Symbol_Data.Symbol then
               Command_Line_Symbols.Table (J) := Symbol_Data;
               Found := True;
               exit;
            end if;
         end loop;

         --  Otherwise, create a new entry in the table

         if not Found then
            Symbol_Table.Increment_Last (Command_Line_Symbols);
            Command_Line_Symbols.Table
              (Symbol_Table.Last (Command_Line_Symbols)) := Symbol_Data;
         end if;
      end loop;
   end Process_Command_Line_Symbol_Definitions;

   -------------------------
   -- Skip_To_End_Of_Line --
   -------------------------

   procedure Skip_To_End_Of_Line is
   begin
      Set_Ignore_Errors (To => True);

      while Token /= Tok_End_Of_Line and then Token /= Tok_EOF loop
         Scan;
      end loop;

      Set_Ignore_Errors (To => False);
   end Skip_To_End_Of_Line;

end Prepcomp;
