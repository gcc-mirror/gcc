------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                  B L D                                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2002-2004 Free Software Foundation, Inc.          --
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

--  This package is still a work in progress.

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;

with Bld.IO;
with Csets;

with GNAT.HTable;
with GNAT.Case_Util;            use GNAT.Case_Util;
with GNAT.Command_Line;         use GNAT.Command_Line;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

with Erroutc;  use Erroutc;
with Err_Vars; use Err_Vars;
with Gnatvsn;
with Namet;    use Namet;
with Opt;      use Opt;
with Output;   use Output;
with Prj;      use Prj;
with Prj.Com;  use Prj.Com;
with Prj.Err;  use Prj.Err;
with Prj.Part;
with Prj.Tree; use Prj.Tree;
with Snames;
with Table;
with Types;    use Types;

package body Bld is

   function "=" (Left, Right : IO.Position) return Boolean
     renames IO."=";

   MAKE_ROOT : constant String := "MAKE_ROOT";

   Process_All_Project_Files : Boolean := True;
   --  Set to False by command line switch -R

   Copyright_Displayed : Boolean := False;
   --  To avoid displaying the Copyright line several times

   Usage_Displayed : Boolean := False;
   --  To avoid displaying the usage several times

   type Expression_Kind_Type is (Undecided, Static_String, Other);

   Expression_Kind : Expression_Kind_Type := Undecided;
   --  After procedure Expression has been called, this global variable
   --  indicates if the expression is a static string or not.
   --  If it is a static string, then Expression_Value (1 .. Expression_Last)
   --  is the static value of the expression.

   Expression_Value  : String_Access := new String (1 .. 10);
   Expression_Last   : Natural := 0;

   --  The following variables indicates if the suffixs and the languages
   --  are statically specified and, if they are, their values.

   C_Suffix          : String_Access := new String (1 .. 10);
   C_Suffix_Last     : Natural := 0;
   C_Suffix_Static   : Boolean := True;

   Cxx_Suffix        : String_Access := new String (1 .. 10);
   Cxx_Suffix_Last   : Natural := 0;
   Cxx_Suffix_Static : Boolean := True;

   Ada_Spec_Suffix        : String_Access := new String (1 .. 10);
   Ada_Spec_Suffix_Last   : Natural := 0;
   Ada_Spec_Suffix_Static : Boolean := True;

   Ada_Body_Suffix        : String_Access := new String (1 .. 10);
   Ada_Body_Suffix_Last   : Natural := 0;
   Ada_Body_Suffix_Static : Boolean := True;

   Languages              : String_Access := new String (1 .. 50);
   Languages_Last         : Natural := 0;
   Languages_Static       : Boolean := True;

   type Source_Kind_Type is (Unknown, Ada_Spec, Ada_Body, C, Cxx, None);
   --  Used when post-processing Compiler'Switches to indicate the language
   --  of a source.

   --  The following variables are used to controlled what attributes
   --  Default_Switches and Switches are allowed in expressions.

   Default_Switches_Package  : Name_Id := No_Name;
   Default_Switches_Language : Name_Id := No_Name;
   Switches_Package          : Name_Id          := No_Name;
   Switches_Language         : Source_Kind_Type := Unknown;

   --  Other attribute references are only allowed in attribute declarations
   --  of the same package and of the same name.

   --  Other_Attribute is True only during attribute declarations other than
   --  Switches or Default_Switches.

   Other_Attribute           : Boolean          := False;
   Other_Attribute_Package   : Name_Id          := No_Name;
   Other_Attribute_Name      : Name_Id          := No_Name;

   type Declaration_Type is (False, May_Be, True);

   Source_Files_Declaration     : Declaration_Type := False;

   Source_List_File_Declaration : Declaration_Type := False;

   --  Names that are not in Snames

   Name_Ide              : Name_Id := No_Name;
   Name_Compiler_Command : Name_Id := No_Name;
   Name_Main_Language    : Name_Id := No_Name;
   Name_C_Plus_Plus      : Name_Id := No_Name;

   package Processed_Projects is new GNAT.HTable.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Project_Node_Id,
      No_Element => Empty_Node,
      Key        => Name_Id,
      Hash       => Hash,
      Equal      => "=");
   --  This hash table contains all processed projects.
   --  It is used to avoid processing the same project file several times.

   package Externals is new GNAT.HTable.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Natural,
      No_Element => 0,
      Key        => Project_Node_Id,
      Hash       => Hash,
      Equal      => "=");
   --  This hash table is used to store all the external references.
   --  For each project file, the tree is first traversed and all
   --  external references are put in variables. Each of these variables
   --  are identified by a number, so that the can be referred to
   --  later during the second traversal of the tree.

   package Variable_Names is new Table.Table
     (Table_Component_Type => Name_Id,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 10,
      Table_Name           => "Bld.Variable_Names");
   --  This table stores all the variables declared in a package.
   --  It is used to distinguish project level and package level
   --  variables identified by simple names.
   --  This table is reset for each package.

   package Switches is new Table.Table
     (Table_Component_Type => Name_Id,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 10,
      Table_Name           => "Bld.Switches");
   --  This table stores all the indexs of associative array attribute
   --  Compiler'Switches specified in a project file. It is reset for
   --  each project file. At the end of processing of a project file
   --  this table is traversed to output targets for those files
   --  that may be C or C++ source files.

   Last_External : Natural := 0;
   --  For each external reference, this variable in incremented by 1,
   --  and a Makefile variable <PROJECT>__EXTERNAL__<Last_External> is
   --  declared. See procedure Process_Externals.

   Last_Case_Construction : Natural := 0;
   --  For each case construction, this variable is incremented by 1,
   --  and a Makefile variable <PROJECT>__CASE__<Last_Case_Construction> is
   --  declared. See procedure Process_Declarative_Items.

   Saved_Suffix : constant String := ".saved";
   --  Prefix to be added to the name of reserved variables (see below) when
   --  used in external references.

   --  A number of environment variables, whose names are used in the
   --  Makefiles are saved at the beginning of the main Makefile.
   --  Each reference to any such environment variable is replaced
   --  in the Makefiles with the name of the saved variable.

   Ada_Body_String      : aliased String := "ADA_BODY";
   Ada_Flags_String     : aliased String := "ADA_FLAGS";
   Ada_Mains_String     : aliased String := "ADA_MAINS";
   Ada_Sources_String   : aliased String := "ADA_SOURCES";
   Ada_Spec_String      : aliased String := "ADA_SPEC";
   Ar_Cmd_String        : aliased String := "AR_CMD";
   Ar_Ext_String        : aliased String := "AR_EXT";
   Base_Dir_String      : aliased String := "BASE_DIR";
   Cc_String            : aliased String := "CC";
   C_Ext_String         : aliased String := "C_EXT";
   Cflags_String        : aliased String := "CFLAGS";
   Cxx_String           : aliased String := "CXX";
   Cxx_Ext_String       : aliased String := "CXX_EXT";
   Cxxflags_String      : aliased String := "CXXFLAGS";
   Deps_Projects_String : aliased String := "DEPS_PROJECT";
   Exec_String          : aliased String := "EXEC";
   Exec_Dir_String      : aliased String := "EXEC_DIR";
   Gnatmake_String      : aliased String := "GNATMAKE";
   Languages_String     : aliased String := "LANGUAGES";
   Ld_Flags_String      : aliased String := "LD_FLAGS";
   Libs_String          : aliased String := "LIBS";
   Main_String          : aliased String := "MAIN";
   Obj_Ext_String       : aliased String := "OBJ_EXT";
   Obj_Dir_String       : aliased String := "OBJ_DIR";
   Project_File_String  : aliased String := "PROJECT_FILE";
   Src_Dirs_String      : aliased String := "SRC_DIRS";

   type Reserved_Variable_Array is array (Positive range <>) of String_Access;
   Reserved_Variables : constant Reserved_Variable_Array :=
     (Ada_Body_String     'Access,
      Ada_Flags_String    'Access,
      Ada_Mains_String    'Access,
      Ada_Sources_String  'Access,
      Ada_Spec_String     'Access,
      Ar_Cmd_String       'Access,
      Ar_Ext_String       'Access,
      Base_Dir_String     'Access,
      Cc_String           'Access,
      C_Ext_String        'Access,
      Cflags_String       'Access,
      Cxx_String          'Access,
      Cxx_Ext_String      'Access,
      Cxxflags_String     'Access,
      Deps_Projects_String'Access,
      Exec_String         'Access,
      Exec_Dir_String     'Access,
      Gnatmake_String     'Access,
      Languages_String    'Access,
      Ld_Flags_String     'Access,
      Libs_String         'Access,
      Main_String         'Access,
      Obj_Ext_String      'Access,
      Obj_Dir_String      'Access,
      Project_File_String 'Access,
      Src_Dirs_String     'Access);

   Main_Project_File_Name : String_Access;
   --  The name of the main project file, given as argument.

   Project_Tree : Project_Node_Id;
   --  The result of the parsing of the main project file.

   procedure Add_To_Expression_Value (S : String);
   procedure Add_To_Expression_Value (S : Name_Id);
   --  Add a string to variable Expression_Value

   procedure Display_Copyright;
   --  Display name of the tool and the copyright

   function Equal_String (Left, Right : Name_Id) return Boolean;
   --  Return True if Left and Right are the same string, without considering
   --  the case.

   procedure Expression
     (Project    : Project_Node_Id;
      First_Term : Project_Node_Id;
      Kind       : Variable_Kind;
      In_Case    : Boolean;
      Reset      : Boolean := False);
   --  Process an expression.
   --  If In_Case is True, all expressions are not static.

   procedure New_Line;
   --  Add a line terminator in the Makefile

   procedure Process (Project : Project_Node_Id);
   --  Process the project tree, result of the parsing.

   procedure Process_Case_Construction
     (Current_Project : Project_Node_Id;
      Current_Pkg     : Name_Id;
      Case_Project    : Project_Node_Id;
      Case_Pkg        : Name_Id;
      Name            : Name_Id;
      Node            : Project_Node_Id);
   --  Process a case construction.
   --  The Makefile declations may be suppressed if no declarative
   --  items in the case items are to be put in the Makefile.

   procedure Process_Declarative_Items
     (Project : Project_Node_Id;
      Pkg     : Name_Id;
      In_Case : Boolean;
      Item    : Project_Node_Id);
   --  Process the declarative items for a project, a package
   --  or a case item.
   --  If In_Case is True, all expressions are not static

   procedure Process_Externals (Project : Project_Node_Id);
   --  Look for all external references in one project file, populate the
   --  table Externals, and output the necessary declarations, if any.

   procedure Put (S : String; With_Substitution : Boolean := False);
   --  Add a string to the Makefile.
   --  When With_Substitution is True, if the string is one of the reserved
   --  variables, replace it with the name of the corresponding saved
   --  variable.

   procedure Put (S : Name_Id);
   --  Add a string to the Makefile.

   procedure Put (P : Positive);
   --  Add the image of a number to the Makefile, without leading space

   procedure Put_Attribute
     (Project : Project_Node_Id;
      Pkg     : Name_Id;
      Name    : Name_Id;
      Index   : Name_Id);
   --  Put the full name of an attribute in the Makefile

   procedure Put_Directory_Separator;
   --  Add a directory separator to the Makefile

   procedure Put_Include_Project
     (Included_Project_Path  : Name_Id;
      Included_Project       : Project_Node_Id;
      Including_Project_Name : String);
   --  Output an include directive for a project

   procedure Put_Line (S : String);
   --  Add a string and a line terminator to the Makefile

   procedure Put_L_Name (N : Name_Id);
   --  Put a name in lower case in the Makefile

   procedure Put_M_Name (N : Name_Id);
   --  Put a name in mixed case in the Makefile

   procedure Put_U_Name (N : Name_Id);
   --  Put a name in upper case in the Makefile

   procedure Special_Put_U_Name (S : Name_Id);
   --  Put a name in upper case in the Makefile.
   --  If "C++" change it to "CXX".

   procedure Put_Variable
     (Project : Project_Node_Id;
      Pkg     : Name_Id;
      Name    : Name_Id);
   --  Put the full name of a variable in the Makefile

   procedure Recursive_Process (Project : Project_Node_Id);
   --  Process a project file and the project files it depends on iteratively
   --  without processing twice the same project file.

   procedure Reset_Suffixes_And_Languages;
   --  Indicate that all suffixes and languages have the default values

   function Source_Kind_Of (File_Name : Name_Id) return Source_Kind_Type;
   --  From a source file name, returns the source kind of the file

   function Suffix_Of
     (Static  : Boolean;
      Value   : String_Access;
      Last    : Natural;
      Default : String) return String;
   --  Returns the current suffix, if it is statically known, or ""
   --  if it is not statically known. Used on C_Suffix, Cxx_Suffix,
   --  Ada_Body_Suffix and Ada_Spec_Suffix.

   procedure Usage;
   --  Display the usage of gnatbuild

   -----------------------------
   -- Add_To_Expression_Value --
   -----------------------------

   procedure Add_To_Expression_Value (S : String) is
   begin
      --  Check that the buffer is large enough.
      --  If it is not, double it until it is large enough.

      while Expression_Last + S'Length > Expression_Value'Last loop
         declare
            New_Value : constant String_Access :=
                          new String (1 .. 2 * Expression_Value'Last);

         begin
            New_Value (1 .. Expression_Last) :=
              Expression_Value (1 .. Expression_Last);
            Free (Expression_Value);
            Expression_Value := New_Value;
         end;
      end loop;

      Expression_Value (Expression_Last + 1 .. Expression_Last + S'Length)
        := S;
      Expression_Last := Expression_Last + S'Length;
   end Add_To_Expression_Value;

   procedure Add_To_Expression_Value (S : Name_Id) is
   begin
      Get_Name_String (S);
      Add_To_Expression_Value (S => Name_Buffer (1 .. Name_Len));
   end Add_To_Expression_Value;

   -----------------------
   -- Display_Copyright --
   -----------------------

   procedure Display_Copyright is
   begin
      if not Copyright_Displayed then
         Copyright_Displayed := True;
         Write_Str ("GPR2MAKE ");
         Write_Str (Gnatvsn.Gnat_Version_String);
         Write_Str (" Copyright 2002-2004 Free Software Foundation, Inc.");
         Write_Eol;
         Write_Eol;
      end if;
   end Display_Copyright;

   ------------------
   -- Equal_String --
   ------------------

   function Equal_String (Left, Right : Name_Id) return Boolean is
   begin
      Get_Name_String (Left);

      declare
         Left_Value : constant String :=
                        To_Lower (Name_Buffer (1 .. Name_Len));

      begin
         Get_Name_String (Right);
         return Left_Value = To_Lower (Name_Buffer (1 .. Name_Len));
      end;
   end Equal_String;

   ----------------
   -- Expression --
   ----------------

   procedure Expression
     (Project    : Project_Node_Id;
      First_Term : Project_Node_Id;
      Kind       : Variable_Kind;
      In_Case    : Boolean;
      Reset      : Boolean := False)
   is
      Term : Project_Node_Id := First_Term;
      --  The term in the expression list

      Current_Term : Project_Node_Id := Empty_Node;
      --  The current term node id

   begin
      if In_Case then
         Expression_Kind := Other;

      elsif Reset then
         Expression_Kind := Undecided;
         Expression_Last := 0;
      end if;

      while Term /= Empty_Node loop

         Current_Term := Tree.Current_Term (Term);

         case Kind_Of (Current_Term) is

            when N_Literal_String =>
               --  If we are in a string list, we precede this literal string
               --  with a space; it does not matter if the output list
               --  has a leading space.
               --  Otherwise we just output the literal string:
               --  if it is not the first term of the expression, it will
               --  concatenate with was previously output.

               if Kind = List then
                  Put (" ");
               end if;

               --  If in a static string expression, add to expression value

               if Expression_Kind = Undecided
                 or else Expression_Kind = Static_String
               then
                  Expression_Kind := Static_String;

                  if Kind = List then
                     Add_To_Expression_Value (" ");
                  end if;

                  Add_To_Expression_Value (String_Value_Of (Current_Term));
               end if;

               Put (String_Value_Of (Current_Term));

            when N_Literal_String_List =>
               --  For string list, we repetedly call Expression with each
               --  element of the list.

               declare
                  String_Node : Project_Node_Id :=
                                  First_Expression_In_List (Current_Term);

               begin
                  if String_Node /= Empty_Node then

                     --  If String_Node is nil, it is an empty list,
                     --  there is nothing to do

                     Expression
                       (Project    => Project,
                        First_Term => Tree.First_Term (String_Node),
                        Kind       => Single,
                        In_Case    => In_Case);

                     loop
                        --  Add the other element of the literal string list
                        --  one after the other

                        String_Node :=
                          Next_Expression_In_List (String_Node);

                        exit when String_Node = Empty_Node;

                        Put (" ");
                        Add_To_Expression_Value (" ");
                        Expression
                          (Project    => Project,
                           First_Term => Tree.First_Term (String_Node),
                           Kind       => Single,
                           In_Case    => In_Case);
                     end loop;
                  end if;
               end;

            when N_Variable_Reference | N_Attribute_Reference =>
               --  A variable or attribute reference is never static

               Expression_Kind := Other;

               --  A variable or an attribute is identified by:
               --   - its project name,
               --   - its package name, if any,
               --   - its name, and
               --   - its index (if an associative array attribute).

               declare
                  Term_Project : Project_Node_Id :=
                                   Project_Node_Of (Current_Term);
                  Term_Package : constant Project_Node_Id :=
                                   Package_Node_Of (Current_Term);

                  Name : constant Name_Id := Name_Of (Current_Term);

                  Term_Package_Name : Name_Id := No_Name;

               begin
                  if Term_Project = Empty_Node then
                     Term_Project := Project;
                  end if;

                  if Term_Package /= Empty_Node then
                     Term_Package_Name := Name_Of (Term_Package);
                  end if;

                  --  If we are in a string list, we precede this variable or
                  --  attribute reference with a space; it does not matter if
                  --  the output list has a leading space.

                  if Kind = List then
                     Put (" ");
                  end if;

                  Put ("$(");

                  if Kind_Of (Current_Term) = N_Variable_Reference then
                     Put_Variable
                       (Project => Term_Project,
                        Pkg     => Term_Package_Name,
                        Name    => Name);

                  else
                     --  Attribute reference.

                     --  If it is a Default_Switches attribute, check if it
                     --  is allowed in this expression (same package and same
                     --  language).

                     if Name = Snames.Name_Default_Switches then
                        if Default_Switches_Package /= Term_Package_Name
                          or else not Equal_String
                                        (Default_Switches_Language,
                                         Associative_Array_Index_Of
                                           (Current_Term))
                        then
                           --  This Default_Switches attribute is not allowed
                           --  here; report an error and continue.
                           --  The Makefiles created will be deleted at the
                           --  end.

                           Error_Msg_Name_1 := Term_Package_Name;
                           Error_Msg
                             ("reference to `%''Default_Switches` " &
                              "not allowed here",
                              Location_Of (Current_Term));
                        end if;

                     --  If it is a Switches attribute, check if it is allowed
                     --  in this expression (same package and same source
                     --  kind).

                     elsif Name = Snames.Name_Switches then
                        if Switches_Package /= Term_Package_Name
                          or else Source_Kind_Of (Associative_Array_Index_Of
                                                    (Current_Term))
                                    /= Switches_Language
                        then
                           --  This Switches attribute is not allowed here;
                           --  report an error and continue. The Makefiles
                           --  created will be deleted at the end.

                           Error_Msg_Name_1 := Term_Package_Name;
                           Error_Msg
                             ("reference to `%''Switches` " &
                              "not allowed here",
                              Location_Of (Current_Term));
                        end if;

                     else
                        --  Other attribute references are only allowed in
                        --  the declaration of an atribute of the same
                        --  package and of the same name.

                        if not Other_Attribute
                          or else Other_Attribute_Package /= Term_Package_Name
                          or else Other_Attribute_Name /= Name
                        then
                           if Term_Package_Name = No_Name then
                              Error_Msg_Name_1 := Name;
                              Error_Msg
                                ("reference to % not allowed here",
                                 Location_Of (Current_Term));

                           else
                              Error_Msg_Name_1 := Term_Package_Name;
                              Error_Msg_Name_2 := Name;
                              Error_Msg
                                ("reference to `%''%` not allowed here",
                                 Location_Of (Current_Term));
                           end if;
                        end if;
                     end if;

                     Put_Attribute
                       (Project => Term_Project,
                        Pkg     => Term_Package_Name,
                        Name    => Name,
                        Index   => Associative_Array_Index_Of (Current_Term));
                  end if;

                  Put (")");
               end;

            when N_External_Value =>
               --  An external reference is never static

               Expression_Kind := Other;

               --  As the external references have already been processed,
               --  we just output the name of the variable that corresponds
               --  to this external reference node.

               Put ("$(");
               Put_U_Name (Name_Of (Project));
               Put (".external.");
               Put (Externals.Get (Current_Term));
               Put (")");

            when others =>

               --  Should never happen

               pragma Assert
                 (False,
                  "illegal node kind in an expression");
               raise Program_Error;
         end case;

         Term := Next_Term (Term);
      end loop;
   end Expression;

   --------------
   -- Gpr2make --
   --------------

   procedure Gpr2make is
   begin
      --  First, get the switches, if any

      loop
         case Getopt ("h q v R") is
            when ASCII.NUL =>
               exit;

            --  -h: Help

            when 'h' =>
               Usage;

            --  -q: Quiet

            when 'q' =>
               Opt.Quiet_Output := True;

            --  -v: Verbose

            when 'v' =>
               Opt.Verbose_Mode := True;
               Display_Copyright;

            --  -R: no Recursivity

            when 'R' =>
               Process_All_Project_Files := False;

            when others =>
               raise Program_Error;
         end case;
      end loop;

      --  Now, get the project file (maximum one)

      loop
         declare
            S : constant String := Get_Argument (Do_Expansion => True);
         begin
            exit when S'Length = 0;

            if Main_Project_File_Name /= null then
               Fail ("only one project file may be specified");

            else
               Main_Project_File_Name := new String'(S);
            end if;
         end;
      end loop;

      --  If no project file specified, display the usage and exit

      if Main_Project_File_Name = null then
         Usage;
         return;
      end if;

      --  Do the necessary initializations

      Csets.Initialize;
      Namet.Initialize;

      Snames.Initialize;

      Prj.Initialize;

      --  Parse the project file(s)

      Prj.Part.Parse (Project_Tree, Main_Project_File_Name.all, False);

      --  If parsing was successful, process the project tree

      if Project_Tree /= Empty_Node then

         --  Create some Name_Ids that are not in Snames

         Name_Len                    := 3;
         Name_Buffer (1 .. Name_Len) := "ide";
         Name_Ide                    := Name_Find;

         Name_Len                    := 16;
         Name_Buffer (1 .. Name_Len) := "compiler_command";
         Name_Compiler_Command       := Name_Find;

         Name_Len                    := 13;
         Name_Buffer (1 .. Name_Len) := "main_language";
         Name_Main_Language          := Name_Find;

         Name_Len                    := 3;
         Name_Buffer (1 .. Name_Len) := "c++";
         Name_C_Plus_Plus            := Name_Find;

         Process (Project_Tree);

         if Compilation_Errors then
            if not Verbose_Mode then
               Write_Eol;
            end if;

            Prj.Err.Finalize;
            Write_Eol;
            IO.Delete_All;
            Fail ("no Makefile created");
         end if;
      end if;
   end Gpr2make;

   --------------
   -- New_Line --
   --------------

   procedure New_Line is
   begin
      IO.New_Line;
   end New_Line;

   -------------
   -- Process --
   -------------

   procedure Process (Project : Project_Node_Id) is
   begin
      Processed_Projects.Reset;
      Recursive_Process (Project);
   end Process;

   -------------------------------
   -- Process_Case_Construction --
   -------------------------------

   procedure Process_Case_Construction
     (Current_Project : Project_Node_Id;
      Current_Pkg     : Name_Id;
      Case_Project    : Project_Node_Id;
      Case_Pkg        : Name_Id;
      Name            : Name_Id;
      Node            : Project_Node_Id)
   is
      Case_Project_Name : constant Name_Id := Name_Of (Case_Project);
      Before            : IO.Position;
      Start             : IO.Position;
      After             : IO.Position;

      procedure Put_Case_Construction;
      --  Output the variable $<PROJECT>__CASE__#, specific to
      --  this case construction. It contains the number of the
      --  branch to follow.

      procedure Recursive_Process
        (Case_Item     : Project_Node_Id;
         Branch_Number : Positive);
      --  A recursive procedure. Calls itself for each branch, increasing
      --  Branch_Number by 1 each time.

      procedure Put_Variable_Name;
      --  Output the case variable

      ---------------------------
      -- Put_Case_Construction --
      ---------------------------

      procedure Put_Case_Construction is
      begin
         Put_U_Name (Case_Project_Name);
         Put (".case.");
         Put (Last_Case_Construction);
      end Put_Case_Construction;

      -----------------------
      -- Recursive_Process --
      -----------------------

      procedure Recursive_Process
        (Case_Item     : Project_Node_Id;
         Branch_Number : Positive)
      is
         Choice_String : Project_Node_Id := First_Choice_Of (Case_Item);

         Before : IO.Position;
         Start  : IO.Position;
         After  : IO.Position;

         No_Lines : Boolean := False;

      begin
         --  Nothing to do if Case_Item is empty.
         --  That should happen only if the case construvtion is totally empty.
         --    case Var is
         --    end case;

         if Case_Item /= Empty_Node then
            --  Remember where we are, to be able to come back here if this
            --  case item is empty.

            IO.Mark (Before);

            if Choice_String = Empty_Node then
               --  when others =>

               --  Output a comment "# when others => ..."

               Put_Line ("# when others => ...");

               --  Remember where we are, to detect if there is anything
               --  put in the Makefile for this branch.

               IO.Mark (Start);

               --  Process the declarative items of this branch

               Process_Declarative_Items
                 (Project => Current_Project,
                  Pkg     => Current_Pkg,
                  In_Case => True,
                  Item    => First_Declarative_Item_Of (Case_Item));

               --  Where are we now?
               IO.Mark (After);

               --  If we are at the same place, the branch is totally empty:
               --  suppress it completely.

               if Start = After then
                  IO.Release (Before);
               end if;
            else
               --  Case Item with one or several case labels

               --  Output a comment
               --    # case <label> => ...
               --  or
               --    # case <first_Label> | ... =>
               --  depending on the number of case labels.

               Put ("# when """);
               Put (String_Value_Of (Choice_String));
               Put ("""");

               if Next_Literal_String (Choice_String) /= Empty_Node then
                  Put (" | ...");
               end if;

               Put (" => ...");
               New_Line;

               --  Check if the case variable is equal to the first case label
               Put ("ifeq ($(");
               Put_Variable_Name;
               Put ("),");
               Put (String_Value_Of (Choice_String));
               Put (")");
               New_Line;

               if Next_Literal_String (Choice_String) /= Empty_Node then
                  --  Several choice strings. We need to use an auxiliary
                  --  variable <PROJECT.case.# to detect if we should follow
                  --  this branch.

                  loop
                     Put_Case_Construction;
                     Put (":=");
                     Put (Branch_Number);
                     New_Line;

                     Put_Line ("endif");

                     Choice_String := Next_Literal_String (Choice_String);

                     exit when Choice_String = Empty_Node;

                     Put ("ifeq ($(");
                     Put_Variable_Name;
                     Put ("),");
                     Put (String_Value_Of (Choice_String));
                     Put (")");
                     New_Line;
                  end loop;

                  --  Now, we test the auxiliary variable

                  Put ("ifeq ($(");
                  Put_Case_Construction;
                  Put ("),");
                  Put (Branch_Number);
                  Put (")");
                  New_Line;
               end if;

               --  Remember where we are before calling
               --  Process_Declarative_Items.

               IO.Mark (Start);

               Process_Declarative_Items
                 (Project => Current_Project,
                  Pkg     => Current_Pkg,
                  In_Case => True,
                  Item    => First_Declarative_Item_Of (Case_Item));

               --  Check where we are now, to detect if some lines have been
               --  added to the Makefile.

               IO.Mark (After);

               No_Lines := Start = After;

               --  If no lines have been added, then suppress completely this
               --  branch.

               if No_Lines then
                  IO.Release (Before);
               end if;

               --  If there is a next branch, process it

               if Next_Case_Item (Case_Item) /= Empty_Node then
                  --  If this branch has not been suppressed, we need an "else"

                  if not No_Lines then
                     --  Mark the position of the "else"

                     IO.Mark (Before);

                     Put_Line ("else");

                     --  Mark the position before the next branch

                     IO.Mark (Start);
                  end if;

                  Recursive_Process
                    (Case_Item => Next_Case_Item (Case_Item),
                     Branch_Number => Branch_Number + 1);

                  if not No_Lines then
                     --  Where are we?
                     IO.Mark (After);

                     --  If we are at the same place, suppress the useless
                     --  "else".

                     if After = Start then
                        IO.Release (Before);
                     end if;
                  end if;
               end if;

               --  If the branch has not been suppressed, we need an "endif"

               if not No_Lines then
                  Put_Line ("endif");
               end if;
            end if;
         end if;
      end Recursive_Process;

      -----------------------
      -- Put_Variable_Name --
      -----------------------

      procedure Put_Variable_Name is
      begin
         Put_Variable (Case_Project, Case_Pkg, Name);
      end Put_Variable_Name;

      --  Start of procedure Process_Case_Construction

   begin
      Last_Case_Construction := Last_Case_Construction + 1;

      --  Remember where we are in case we suppress completely the case
      --  construction.

      IO.Mark (Before);

      New_Line;

      --  Output a comment line for this case construction

      Put ("# case ");
      Put_M_Name (Case_Project_Name);

      if Case_Pkg /= No_Name then
         Put (".");
         Put_M_Name (Case_Pkg);
      end if;

      Put (".");
      Put_M_Name (Name);
      Put (" is ...");
      New_Line;

      --  Remember where we are, to detect if all branches have been suppressed

      IO.Mark (Start);

      --  Start at the first case item

      Recursive_Process
        (Case_Item     => First_Case_Item_Of (Node),
         Branch_Number => 1);

      --  Where are we?

      IO.Mark (After);

      --  If we are at the same position, it means that all branches have been
      --  suppressed: then we suppress completely the case construction.

      if Start = After then
         IO.Release (Before);

      else
         --  If the case construction is not completely suppressed, we issue
         --  a comment indicating the end of the case construction.

         Put_Line ("# end case;");

         New_Line;
      end if;
   end Process_Case_Construction;

   -------------------------------
   -- Process_Declarative_Items --
   -------------------------------

   procedure Process_Declarative_Items
     (Project : Project_Node_Id;
      Pkg     : Name_Id;
      In_Case : Boolean;
      Item    : Project_Node_Id)
   is
      Current_Declarative_Item : Project_Node_Id := Item;
      Current_Item             : Project_Node_Id := Empty_Node;

      Project_Name : constant String :=
                       To_Upper (Get_Name_String (Name_Of (Project)));
      Item_Name    : Name_Id := No_Name;

   begin
      --  For each declarative item

      while Current_Declarative_Item /= Empty_Node loop
         --  Get its data

         Current_Item := Current_Item_Node (Current_Declarative_Item);

         --  And set Current_Declarative_Item to the next declarative item
         --  ready for the next iteration

         Current_Declarative_Item := Next_Declarative_Item
                                            (Current_Declarative_Item);

         --  By default, indicate that we are not declaring attribute
         --  Default_Switches or Switches.

         Other_Attribute := False;

         --  Write_Line (Project_Node_Kind'Image (Kind_Of (Current_Item)));

         case Kind_Of (Current_Item) is

            when N_Package_Declaration =>
               Item_Name := Name_Of (Current_Item);

               declare
                  Real_Project : constant Project_Node_Id :=
                                   Project_Of_Renamed_Package_Of
                                     (Current_Item);

                  Before_Package   : IO.Position;
                  Start_Of_Package : IO.Position;
                  End_Of_Package   : IO.Position;

                  Decl_Item : Project_Node_Id;

               begin
                  --  If it is a renaming package, we go to the original
                  --  package. This is guaranteed to work, otherwise the
                  --  parsing of the project file tree would have already
                  --  failed.

                  if Real_Project /= Empty_Node then
                     Decl_Item :=
                       First_Declarative_Item_Of
                         (Project_Declaration_Of (Real_Project));

                     --  Traverse the declarative items of the project,
                     --  until we find the renamed package.

                     while Decl_Item /= Empty_Node loop
                        Current_Item := Current_Item_Node (Decl_Item);
                        exit when Kind_Of (Current_Item)
                                   = N_Package_Declaration
                                  and then Name_Of (Current_Item) = Item_Name;
                        Decl_Item := Next_Declarative_Item (Decl_Item);
                     end loop;
                  end if;

                  --  Remember where we are, in case we want to completely
                  --  suppress this package.

                  IO.Mark (Before_Package);

                  New_Line;

                  --  Output comment line for this package

                  Put ("# package ");
                  Put_M_Name (Item_Name);
                  Put (" is ...");
                  New_Line;

                  --  Record where we are before calling
                  --  Process_Declarative_Items.

                  IO.Mark (Start_Of_Package);

                  --  And process the declarative items of this package

                  Process_Declarative_Items
                    (Project => Project,
                     Pkg     => Item_Name,
                     In_Case => False,
                     Item    => First_Declarative_Item_Of (Current_Item));

                  --  Reset the local variables once we have finished with
                  --  this package.

                  Variable_Names.Init;

                  --  Where are we?
                  IO.Mark (End_Of_Package);

                  --  If we are at the same place, suppress completely the
                  --  package.

                  if End_Of_Package = Start_Of_Package then
                     IO.Release (Before_Package);

                  else

                     --  otherwise, utput comment line for end of package

                     Put ("# end ");
                     Put_M_Name (Item_Name);
                     Put (";");
                     New_Line;

                     New_Line;
                  end if;
               end;

            when N_Variable_Declaration | N_Typed_Variable_Declaration =>
               Item_Name := Name_Of (Current_Item);

               --  Output comment line for this variable

               Put ("# ");
               Put_M_Name (Item_Name);
               Put (" := ...");
               New_Line;

               --  If we are inside a package, the variable is a local
               --  variable, not a project level variable.
               --  So we check if its name is included in the Variables
               --  table; if it is not already, we put it in the table.

               if Pkg /= No_Name then
                  declare
                     Found : Boolean := False;

                  begin
                     for
                       Index in Variable_Names.First .. Variable_Names.Last
                     loop
                        if Variable_Names.Table (Index) = Item_Name then
                           Found := True;
                           exit;
                        end if;
                     end loop;

                     if not Found then
                        Variable_Names.Increment_Last;
                        Variable_Names.Table (Variable_Names.Last) :=
                          Item_Name;
                     end if;
                  end;
               end if;

               --  Output the line <variable_Name>:=<expression>

               Put_Variable (Project, Pkg, Item_Name);

               Put (":=");

               Expression
                 (Project    => Project,
                  First_Term => Tree.First_Term (Expression_Of (Current_Item)),
                  Kind       => Expression_Kind_Of (Current_Item),
                  In_Case    => In_Case);

               New_Line;

            when N_Attribute_Declaration =>
               Item_Name := Name_Of (Current_Item);

               declare
                  Index : constant Name_Id :=
                            Associative_Array_Index_Of (Current_Item);

                  Pos_Comment     : IO.Position;
                  Put_Declaration : Boolean := True;

               begin
                  --  If it is a Default_Switches attribute register the
                  --  project, the package and the language to indicate
                  --  what Default_Switches attribute references are allowed
                  --  in expressions.

                  if Item_Name = Snames.Name_Default_Switches then
                     Default_Switches_Package  := Pkg;
                     Default_Switches_Language := Index;

                  --  If it is a Switches attribute register the project,
                  --  the package and the source kind to indicate what
                  --  Switches attribute references are allowed in expressions.

                  elsif Item_Name = Snames.Name_Switches then
                     Switches_Package  := Pkg;
                     Switches_Language := Source_Kind_Of (Index);

                  else
                     --  Set Other_Attribute to True to indicate that we are
                     --  in the declaration of an attribute other than
                     --  Switches or Default_Switches.

                     Other_Attribute         := True;
                     Other_Attribute_Package := Pkg;
                     Other_Attribute_Name    := Item_Name;
                  end if;

                  --  Record where we are to be able to suppress the
                  --  declaration.

                  IO.Mark (Pos_Comment);

                  --  Output comment line for this attribute

                  Put ("# for ");
                  Put_M_Name (Item_Name);

                  if Index /= No_Name then
                     Put (" (""");
                     Put (Index);
                     Put (""")");
                  end if;

                  Put (" use ...");
                  New_Line;

                  --  Output the line <attribute_name>:=<expression>

                  Put_Attribute (Project, Pkg, Item_Name, Index);
                  Put (":=");
                  Expression
                    (Project    => Project,
                     First_Term =>
                       Tree.First_Term (Expression_Of (Current_Item)),
                     Kind        => Expression_Kind_Of (Current_Item),
                     In_Case     => In_Case,
                     Reset       => True);
                  New_Line;

                  --  Remove any Default_Switches attribute declaration for
                  --  languages other than C or C++.

                  if Item_Name = Snames.Name_Default_Switches then
                     Get_Name_String (Index);
                     To_Lower (Name_Buffer (1 .. Name_Len));
                     Put_Declaration :=
                       Name_Buffer (1 .. Name_Len) = "c" or else
                       Name_Buffer (1 .. Name_Len) = "c++";

                  --  Remove any Switches attribute declaration for source
                  --  kinds other than C, C++ or unknown.

                  elsif Item_Name = Snames.Name_Switches then
                     Put_Declaration :=
                       Switches_Language = Unknown
                       or else Switches_Language = C
                       or else Switches_Language = Cxx;

                  end if;

                  --  Attributes in packages other than Naming, Compiler or
                  --  IDE are of no interest; suppress their declarations.

                  Put_Declaration := Put_Declaration and
                    (Pkg = No_Name
                       or else Pkg = Snames.Name_Naming
                       or else Pkg = Snames.Name_Compiler
                       or else Pkg = Name_Ide);

                  if Put_Declaration then
                     --  Some attributes are converted into reserved variables

                     if Pkg = No_Name then

                        --  Project level attribute

                        if Item_Name = Snames.Name_Languages then

                           --  for Languages use ...

                           --  Attribute Languages is converted to variable
                           --  LANGUAGES. The actual string is put in lower
                           --  case.

                           Put ("LANGUAGES:=");

                           --  If the expression is static (expected to be so
                           --  most of the cases), then just give to LANGUAGES
                           --  the lower case value of the expression.

                           if Expression_Kind = Static_String then
                              Put (To_Lower (Expression_Value
                                               (1 .. Expression_Last)));

                           else
                              --  Otherwise, call to_lower on the value
                              --  of the attribute.

                              Put ("$(shell gprcmd to_lower $(");
                              Put_Attribute
                                (Project, No_Name, Item_Name, No_Name);
                              Put ("))");
                           end if;

                           New_Line;

                           --  Record value of Languages if expression is
                           --  static and if Languages_Static is True.

                           if Expression_Kind /= Static_String then
                              Languages_Static := False;

                           elsif Languages_Static then
                              To_Lower
                                (Expression_Value (1 .. Expression_Last));

                              if Languages_Last = 0 then
                                 if Languages'Last < Expression_Last + 2 then
                                    Free (Languages);
                                    Languages :=
                                      new String (1 .. Expression_Last + 2);
                                 end if;

                                 Languages (1) := ' ';
                                 Languages (2 .. Expression_Last + 1) :=
                                   Expression_Value (1 .. Expression_Last);
                                 Languages_Last := Expression_Last + 2;
                                 Languages (Languages_Last) := ' ';

                              else
                                 Languages_Static :=
                                   Languages (2 .. Languages_Last - 1) =
                                   Expression_Value (1 .. Expression_Last);
                              end if;
                           end if;

                        elsif Item_Name = Snames.Name_Source_Dirs then

                           --  for Source_Dirs use ...

                           --  String list attribute Source_Dirs is converted
                           --  to variable <PROJECT>.src_dirs, each element
                           --  being an absolute directory name.

                           Put (Project_Name &
                                ".src_dirs:=$(shell gprcmd extend $(");
                           Put (Project_Name);
                           Put (".base_dir) '$(");
                           Put_Attribute (Project, Pkg, Item_Name, No_Name);
                           Put_Line (")')");

                        elsif Item_Name = Snames.Name_Source_Files then

                           --  for Source_Files use ...

                           --  String list Source_Files is converted to
                           --  variable <PROJECT>.src_files

                           Put (Project_Name);
                           Put (".src_files:=$(");
                           Put_Attribute (Project, Pkg, Item_Name, No_Name);
                           Put (")");
                           New_Line;

                           if In_Case then
                              if Source_Files_Declaration = False then
                                 Source_Files_Declaration := May_Be;
                              end if;

                              if Source_Files_Declaration /= True then

                                 --  Variable src_files.specified is set to
                                 --  TRUE. It will be tested to decide if there
                                 --  is a need to look for source files either
                                 --  in the source directories or in a source
                                 --  list file.

                                 Put_Line ("src_files.specified:=TRUE");
                              end if;

                           else
                              Source_Files_Declaration := True;
                           end if;

                        elsif Item_Name = Snames.Name_Source_List_File then

                           --  for Source_List_File use ...

                           --  Single string Source_List_File is converted to
                           --  variable src.list_file. It will be used
                           --  later, if necessary, to get the source
                           --  file names from the specified file.
                           --  The file name is converted to an absolute path
                           --  name if necessary.

                           Put ("src.list_file:=" &
                                "$(strip $(shell gprcmd to_absolute $(");
                           Put (Project_Name);
                           Put (".base_dir) '$(");
                           Put_Attribute (Project, Pkg, Item_Name, No_Name);
                           Put_Line (")'))");

                           if In_Case then
                              if Source_List_File_Declaration = False then
                                 Source_List_File_Declaration := May_Be;
                              end if;

                              if Source_Files_Declaration /= True
                                and then Source_List_File_Declaration /= True
                              then
                                 --  Variable src_list_file.specified is set to
                                 --  TRUE. It will be tested later, if
                                 --  necessary, to read the source list file.

                                 Put_Line ("src_list_file.specified:=TRUE");
                              end if;

                           else
                              Source_List_File_Declaration := True;
                           end if;

                        elsif Item_Name = Snames.Name_Object_Dir then

                           --  for Object_Dir use ...

                           --  Single string attribute Object_Dir is converted
                           --  to variable <PROJECT>.obj_dir. The directory is
                           --  converted to an absolute path name,
                           --  if necessary.

                           Put (Project_Name);
                           Put (".obj_dir:=" &
                                "$(strip $(shell gprcmd to_absolute $(");
                           Put (Project_Name);
                           Put (".base_dir) '$(");
                           Put_Attribute (Project, Pkg, Item_Name, No_Name);
                           Put_Line (")'))");

                        elsif Item_Name = Snames.Name_Exec_Dir then

                           --  for Exec_Dir use ...

                           --  Single string attribute Exec_Dir is converted
                           --  to variable EXEC_DIR. The directory is
                           --  converted to an absolute path name,
                           --  if necessary.

                           Put ("EXEC_DIR:=" &
                                "$(strip $(shell gprcmd to_absolute $(");
                           Put (Project_Name);
                           Put (".base_dir) '$(");
                           Put_Attribute (Project, Pkg, Item_Name, No_Name);
                           Put_Line (")'))");

                        elsif Item_Name = Snames.Name_Main then

                           --  for Mains use ...

                           --  String list attribute Main is converted to
                           --  variable ADA_MAINS.

                           Put ("ADA_MAINS:=$(");
                           Put_Attribute (Project, Pkg, Item_Name, No_Name);
                           Put (")");
                           New_Line;

                        elsif Item_Name = Name_Main_Language then

                           --  for Main_Language use ...

                           Put ("MAIN:=");

                           --  If the expression is static (expected to be so
                           --  most of the cases), then just give to MAIN
                           --  the lower case value of the expression.

                           if Expression_Kind = Static_String then
                              Put (To_Lower (Expression_Value
                                               (1 .. Expression_Last)));

                           else
                              --  Otherwise, call to_lower on the value
                              --  of the attribute.

                              Put ("$(shell gprcmd to_lower $(");
                              Put_Attribute
                                (Project, No_Name, Item_Name, No_Name);
                              Put ("))");
                           end if;

                           New_Line;

                        else
                           --  Other attribute are of no interest; suppress
                           --  their declarations.

                           Put_Declaration := False;
                        end if;

                     elsif Pkg = Snames.Name_Compiler then
                        --  Attribute of package Compiler

                        if Item_Name = Snames.Name_Switches then

                           --  for Switches (<file_name>) use ...

                           --  As the C and C++ extension may not be known
                           --  statically, at the end of the processing of this
                           --  project file, a test will done to decide if the
                           --  file name (the index) has a C or C++ extension.
                           --  The index is recorded in the table Switches,
                           --  making sure that it appears only once.

                           declare
                              Found : Boolean := False;
                           begin
                              for J in Switches.First .. Switches.Last loop
                                 if Switches.Table (J) = Index then
                                    Found := True;
                                    exit;
                                 end if;
                              end loop;

                              if not Found then
                                 Switches.Increment_Last;
                                 Switches.Table (Switches.Last) := Index;
                              end if;
                           end;

                        elsif Item_Name = Snames.Name_Default_Switches then
                           Get_Name_String (Index);
                           To_Lower (Name_Buffer (1 .. Name_Len));

                           if Name_Buffer (1 .. Name_Len) = "c" then
                              Put ("CFLAGS:=$(");
                              Put_Attribute (Project, Pkg, Item_Name, Index);
                              Put (")");
                              New_Line;

                           elsif Name_Buffer (1 .. Name_Len) = "c++" then
                              Put ("CXXFLAGS:=$(");
                              Put_Attribute (Project, Pkg, Item_Name, Index);
                              Put (")");
                              New_Line;
                           end if;
                        else
                           --  Other attribute are of no interest; suppress
                           --  their declarations.

                           Put_Declaration := False;
                        end if;

                     elsif Pkg = Name_Ide then

                        --  Attributes of package IDE

                        if Item_Name = Name_Compiler_Command then

                           --  for Compiler_Command (<language>) use ...

                           declare
                              Index_Name : Name_Id := No_Name;

                           begin
                              Get_Name_String (Index);
                              To_Lower (Name_Buffer (1 .. Name_Len));
                              Index_Name := Name_Find;

                              --  Only "Ada", "C" and "C++" are of interest

                              if Index_Name = Snames.Name_Ada then

                                 --  For "Ada", we set the variable $GNATMAKE

                                 Put ("GNATMAKE:=$(");
                                 Put_Attribute
                                   (Project, Pkg, Item_Name, Index);
                                 Put (")");
                                 New_Line;

                              elsif Index_Name = Snames.Name_C then

                                 --  For "C", we set the variable $CC

                                 Put ("CC:=$(");
                                 Put_Attribute
                                   (Project, Pkg, Item_Name, Index);
                                 Put (")");
                                 New_Line;

                              elsif Index_Name = Name_C_Plus_Plus then

                                 --  For "C++", we set the variable $CXX

                                 Put ("CXX:=$(");
                                 Put_Attribute
                                   (Project, Pkg, Item_Name, Index);
                                 Put (")");
                                 New_Line;
                              end if;
                           end;
                        else
                           --  Other attribute are of no interest; suppress
                           --  their declarations.

                           Put_Declaration := False;
                        end if;

                     elsif Pkg = Snames.Name_Naming then
                        --  Attributes of package Naming

                        if Item_Name = Snames.Name_Body_Suffix then

                           --  for Body_Suffix (<language>) use ...

                           declare
                              Index_Name : Name_Id := No_Name;

                           begin
                              Get_Name_String (Index);
                              To_Lower (Name_Buffer (1 .. Name_Len));
                              Index_Name := Name_Find;

                              --  Languages "C", "C++" & "Ada" are of interest

                              if Index_Name = Snames.Name_C then

                                 --  For "C", we set the variable C_EXT

                                 Put ("C_EXT:=$(");
                                 Put_Attribute
                                   (Project, Pkg, Item_Name, Index);
                                 Put (")");
                                 New_Line;

                                 if Expression_Kind /= Static_String then
                                    C_Suffix_Static := False;

                                 elsif C_Suffix_Static then
                                    if C_Suffix_Last = 0 then
                                       if C_Suffix'Last < Expression_Last then
                                          Free (C_Suffix);
                                          C_Suffix := new String'
                                            (Expression_Value
                                               (1 .. Expression_Last));

                                       else
                                          C_Suffix (1 .. Expression_Last) :=
                                            Expression_Value
                                            (1 .. Expression_Last);
                                       end if;

                                       C_Suffix_Last := Expression_Last;

                                    else
                                       C_Suffix_Static :=
                                         Expression_Value
                                           (1 .. Expression_Last) =
                                         C_Suffix (1 .. C_Suffix_Last);
                                    end if;
                                 end if;

                              elsif Index_Name = Name_C_Plus_Plus then

                                 --  For "C++", we set the variable CXX_EXT

                                 Put ("CXX_EXT:=$(");
                                 Put_Attribute
                                   (Project, Pkg, Item_Name, Index);
                                 Put (")");
                                 New_Line;

                                 if Expression_Kind /= Static_String then
                                    Cxx_Suffix_Static := False;

                                 elsif Cxx_Suffix_Static then
                                    if Cxx_Suffix_Last = 0 then
                                       if
                                         Cxx_Suffix'Last < Expression_Last
                                       then
                                          Free (Cxx_Suffix);
                                          Cxx_Suffix := new String'
                                            (Expression_Value
                                               (1 .. Expression_Last));

                                       else
                                          Cxx_Suffix (1 .. Expression_Last) :=
                                            Expression_Value
                                            (1 .. Expression_Last);
                                       end if;

                                       Cxx_Suffix_Last := Expression_Last;

                                    else
                                       Cxx_Suffix_Static :=
                                         Expression_Value
                                           (1 .. Expression_Last) =
                                         Cxx_Suffix (1 .. Cxx_Suffix_Last);
                                    end if;
                                 end if;

                              elsif Index_Name = Snames.Name_Ada then

                                 --  For "Ada", we set the variable ADA_BODY

                                 Put ("ADA_BODY:=$(");
                                 Put_Attribute
                                   (Project, Pkg, Item_Name, Index);
                                 Put (")");
                                 New_Line;

                                 if Expression_Kind /= Static_String then
                                    Ada_Body_Suffix_Static := False;

                                 elsif Ada_Body_Suffix_Static then
                                    if Ada_Body_Suffix_Last = 0 then
                                       if
                                         Ada_Body_Suffix'Last < Expression_Last
                                       then
                                          Free (Ada_Body_Suffix);
                                          Ada_Body_Suffix := new String'
                                            (Expression_Value
                                               (1 .. Expression_Last));

                                       else
                                          Ada_Body_Suffix
                                            (1 .. Expression_Last) :=
                                            Expression_Value
                                              (1 .. Expression_Last);
                                       end if;

                                       Ada_Body_Suffix_Last := Expression_Last;

                                    else
                                       Ada_Body_Suffix_Static :=
                                         Expression_Value
                                           (1 .. Expression_Last) =
                                           Ada_Body_Suffix
                                             (1 .. Ada_Body_Suffix_Last);
                                    end if;
                                 end if;
                              end if;
                           end;

                        elsif Item_Name = Snames.Name_Spec_Suffix then

                           --  for Spec_Suffix (<language>) use ...

                           declare
                              Index_Name : Name_Id := No_Name;

                           begin
                              Get_Name_String (Index);
                              To_Lower (Name_Buffer (1 .. Name_Len));
                              Index_Name := Name_Find;

                              --  Only "Ada" is of interest

                              if Index_Name = Snames.Name_Ada then

                                 --  For "Ada", we set the variable ADA_SPEC

                                 Put ("ADA_SPEC:=$(");
                                 Put_Attribute
                                   (Project, Pkg, Item_Name, Index);
                                 Put (")");
                                 New_Line;

                                 if Expression_Kind /= Static_String then
                                    Ada_Spec_Suffix_Static := False;

                                 elsif Ada_Spec_Suffix_Static then
                                    if Ada_Spec_Suffix_Last = 0 then
                                       if
                                         Ada_Spec_Suffix'Last < Expression_Last
                                       then
                                          Free (Ada_Spec_Suffix);
                                          Ada_Spec_Suffix := new String'
                                            (Expression_Value
                                               (1 .. Expression_Last));

                                       else
                                          Ada_Spec_Suffix
                                            (1 .. Expression_Last) :=
                                            Expression_Value
                                              (1 .. Expression_Last);
                                       end if;

                                       Ada_Spec_Suffix_Last := Expression_Last;

                                    else
                                       Ada_Spec_Suffix_Static :=
                                         Expression_Value
                                         (1 .. Expression_Last) =
                                         Ada_Spec_Suffix
                                         (1 .. Ada_Spec_Suffix_Last);
                                    end if;
                                 end if;
                              end if;
                           end;
                        else
                           --  Other attribute are of no interest; suppress
                           --  their declarations.

                           Put_Declaration := False;
                        end if;
                     end if;
                  end if;

                  --  Suppress the attribute declaration if not needed

                  if not Put_Declaration then
                     IO.Release (Pos_Comment);
                  end if;
               end;

            when N_Case_Construction =>

               --  case <typed_string_variable> is ...

               declare
                  Case_Project  : Project_Node_Id := Project;
                  Case_Pkg      : Name_Id := No_Name;
                  Variable_Node : constant Project_Node_Id :=
                                    Case_Variable_Reference_Of (Current_Item);
                  Variable_Name : constant Name_Id := Name_Of (Variable_Node);

               begin
                  if Project_Node_Of (Variable_Node) /= Empty_Node then
                     Case_Project := Project_Node_Of (Variable_Node);
                  end if;

                  if Package_Node_Of (Variable_Node) /= Empty_Node then
                     Case_Pkg := Name_Of (Package_Node_Of (Variable_Node));
                  end if;

                  --  If we are in a package, and no package is specified
                  --  for the case variable, we look into the table
                  --  Variables_Names to decide if it is a variable local
                  --  to the package or a project level variable.

                  if Pkg /= No_Name
                    and then Case_Pkg = No_Name
                    and then Case_Project = Project
                  then
                     for
                       Index in Variable_Names.First .. Variable_Names.Last
                     loop
                        if Variable_Names.Table (Index) = Variable_Name then
                           Case_Pkg := Pkg;
                           exit;
                        end if;
                     end loop;
                  end if;

                  --  The real work is done in Process_Case_Construction.

                  Process_Case_Construction
                    (Current_Project => Project,
                     Current_Pkg     => Pkg,
                     Case_Project    => Case_Project,
                     Case_Pkg        => Case_Pkg,
                     Name            => Variable_Name,
                     Node            => Current_Item);
               end;

            when others =>
               null;

         end case;
      end loop;
   end Process_Declarative_Items;

   -----------------------
   -- Process_Externals --
   -----------------------
   procedure Process_Externals (Project : Project_Node_Id) is
      Project_Name : constant Name_Id := Name_Of (Project);

      No_External_Yet : Boolean := True;

      procedure Expression (First_Term : Project_Node_Id);
      --  Look for external reference in the term of an expression.
      --  If one is found, build the Makefile external reference variable.

      procedure Process_Declarative_Items (Item : Project_Node_Id);
      --  Traverse the declarative items of a project file to find all
      --  external references.

      ----------------
      -- Expression --
      ----------------

      procedure Expression (First_Term : Project_Node_Id) is
         Term : Project_Node_Id := First_Term;
         --  The term in the expression list

         Current_Term : Project_Node_Id := Empty_Node;
         --  The current term node id

         Default : Project_Node_Id;

      begin
         --  Check each term of the expression

         while Term /= Empty_Node loop
            Current_Term := Tree.Current_Term (Term);

            if Kind_Of (Current_Term) = N_External_Value then

               --  If it is the first external reference of this project file,
               --  output a comment

               if No_External_Yet then
                  No_External_Yet := False;
                  New_Line;

                  Put_Line ("# external references");

                  New_Line;
               end if;

               --  Increase Last_External and record the node of the external
               --  reference in table Externals, so that the external reference
               --  variable can be identified later.

               Last_External := Last_External + 1;
               Externals.Set (Current_Term, Last_External);

               Default := External_Default_Of (Current_Term);

               Get_Name_String
                 (String_Value_Of (External_Reference_Of (Current_Term)));

               declare
                  External_Name : constant String :=
                                    Name_Buffer (1 .. Name_Len);

               begin
                  --  Output a comment for this external reference

                  Put ("# external (""");
                  Put (External_Name);

                  if Default /= Empty_Node then
                     Put (""", """);
                     Put (String_Value_Of (Default));
                  end if;

                  Put (""")");
                  New_Line;

                  --  If there is no default, output one line:

                  --  <PROJECT>__EXTERNAL__#:=$(<external name>)

                  if Default = Empty_Node then
                     Put_U_Name (Project_Name);
                     Put (".external.");
                     Put (Last_External);
                     Put (":=$(");
                     Put (External_Name, With_Substitution => True);
                     Put (")");
                     New_Line;

                  else
                     --  When there is a default, output the following lines:

                     --  ifeq ($(<external_name),)
                     --     <PROJECT>__EXTERNAL__#:=<default>
                     --  else
                     --     <PROJECT>__EXTERNAL__#:=$(<external_name>)
                     --  endif

                     Put ("ifeq ($(");
                     Put (External_Name, With_Substitution => True);
                     Put ("),)");
                     New_Line;

                     Put ("   ");
                     Put_U_Name (Project_Name);
                     Put (".external.");
                     Put (Last_External);
                     Put (":=");
                     Put (String_Value_Of (Default));
                     New_Line;

                     Put_Line ("else");

                     Put ("   ");
                     Put_U_Name (Project_Name);
                     Put (".external.");
                     Put (Last_External);
                     Put (":=$(");
                     Put (External_Name, With_Substitution => True);
                     Put (")");
                     New_Line;

                     Put_Line ("endif");
                  end if;
               end;
            end if;

            Term := Next_Term (Term);
         end loop;
      end Expression;

      -------------------------------
      -- Process_Declarative_Items --
      -------------------------------

      procedure Process_Declarative_Items (Item : Project_Node_Id) is
         Current_Declarative_Item : Project_Node_Id := Item;
         Current_Item             : Project_Node_Id := Empty_Node;

      begin
         --  For each declarative item

         while Current_Declarative_Item /= Empty_Node loop
            Current_Item := Current_Item_Node (Current_Declarative_Item);

            --  Set Current_Declarative_Item to the next declarative item
            --  ready for the next iteration

            Current_Declarative_Item := Next_Declarative_Item
                                          (Current_Declarative_Item);

            --  Write_Line (Project_Node_Kind'Image (Kind_Of (Current_Item)));

            case Kind_Of (Current_Item) is

               when N_Package_Declaration =>

                  --  Recursive call the declarative items of a package

                  if
                    Project_Of_Renamed_Package_Of (Current_Item) = Empty_Node
                  then
                     Process_Declarative_Items
                       (First_Declarative_Item_Of (Current_Item));
                  end if;

               when N_Attribute_Declaration      |
                    N_Typed_Variable_Declaration |
                    N_Variable_Declaration        =>

                  --  Process the expression to look for external references

                  Expression
                    (First_Term => Tree.First_Term
                                      (Expression_Of (Current_Item)));

               when N_Case_Construction =>

                  --  Recursive calls to process the declarative items of
                  --  each case item.

                  declare
                     Case_Item : Project_Node_Id :=
                       First_Case_Item_Of (Current_Item);

                  begin
                     while Case_Item /= Empty_Node loop
                        Process_Declarative_Items
                          (First_Declarative_Item_Of (Case_Item));
                        Case_Item := Next_Case_Item (Case_Item);
                     end loop;
                  end;

               when others =>
                  null;
            end case;
         end loop;
      end Process_Declarative_Items;

      --  Start of procedure Process_Externals

   begin
      Process_Declarative_Items
        (First_Declarative_Item_Of (Project_Declaration_Of (Project)));

      if not No_External_Yet then
         Put_Line ("# end of external references");
         New_Line;
      end if;
   end Process_Externals;

   ---------
   -- Put --
   ---------

   procedure Put (S : String; With_Substitution : Boolean := False) is
   begin
      IO.Put (S);

      --  If With_Substitution is True, check if S is one of the reserved
      --  variables. If it is, append to it the Saved_Suffix.

      if With_Substitution then
         for J in Reserved_Variables'Range loop
            if S = Reserved_Variables (J).all then
               IO.Put (Saved_Suffix);
               exit;
            end if;
         end loop;
      end if;
   end Put;

   procedure Put (P : Positive) is
      Image : constant String := P'Img;

   begin
      Put (Image (Image'First + 1 .. Image'Last));
   end Put;

   procedure Put (S : Name_Id) is
   begin
      Get_Name_String (S);
      Put (Name_Buffer (1 .. Name_Len));
   end Put;

   -------------------
   -- Put_Attribute --
   -------------------

   procedure Put_Attribute
     (Project : Project_Node_Id;
      Pkg     : Name_Id;
      Name    : Name_Id;
      Index   : Name_Id)
   is
   begin
      Put_U_Name (Name_Of (Project));

      if Pkg /= No_Name then
         Put (".");
         Put_L_Name (Pkg);
      end if;

      Put (".");
      Put_L_Name (Name);

      if Index /= No_Name then
         Put (".");

         --  For attribute Switches, we don't want to change the file name

         if Name = Snames.Name_Switches then
            Get_Name_String (Index);
            Put (Name_Buffer (1 .. Name_Len));

         else
            Special_Put_U_Name (Index);
         end if;
      end if;
   end Put_Attribute;

   -----------------------------
   -- Put_Directory_Separator --
   -----------------------------

   procedure Put_Directory_Separator is
   begin
      Put (S => (1 => Directory_Separator));
   end Put_Directory_Separator;

   -------------------------
   -- Put_Include_Project --
   -------------------------

   procedure Put_Include_Project
     (Included_Project_Path  : Name_Id;
      Included_Project       : Project_Node_Id;
      Including_Project_Name : String)
   is
   begin
      --  If path is null, there is nothing to do.
      --  This happens when there is no project being extended.

      if Included_Project_Path /= No_Name then
         Get_Name_String (Included_Project_Path);

         declare
            Included_Project_Name : constant String :=
              Get_Name_String (Name_Of (Included_Project));
            Included_Directory_Path : constant String :=
              Dir_Name (Name_Buffer (1 .. Name_Len));
            Last : Natural := Included_Directory_Path'Last;

         begin
            --  Remove a possible directory separator at the end of the
            --  directory.

            if Last >= Included_Directory_Path'First
              and then Included_Directory_Path (Last) = Directory_Separator
            then
               Last := Last - 1;
            end if;

            Put ("BASE_DIR=");

            --  If it is a relative path, precede the directory with
            --  $(<PROJECT>.base_dir)/

            if not Is_Absolute_Path (Included_Directory_Path) then
               Put ("$(");
               Put (Including_Project_Name);
               Put (".base_dir)" & Directory_Separator);
            end if;

            Put (Included_Directory_Path
                   (Included_Directory_Path'First .. Last));
            New_Line;

            --  Include the Makefile

            Put ("include $(BASE_DIR)");
            Put_Directory_Separator;
            Put ("Makefile.");
            Put (To_Lower (Included_Project_Name));
            New_Line;

            New_Line;
         end;
      end if;
   end Put_Include_Project;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (S : String) is
   begin
      IO.Put (S);
      IO.New_Line;
   end Put_Line;

   ----------------
   -- Put_L_Name --
   ----------------

   procedure Put_L_Name (N : Name_Id) is
   begin
      Put (To_Lower (Get_Name_String (N)));
   end Put_L_Name;

   ----------------
   -- Put_M_Name --
   ----------------

   procedure Put_M_Name (N : Name_Id) is
      Name : String := Get_Name_String (N);

   begin
      To_Mixed (Name);
      Put (Name);
   end Put_M_Name;

   ----------------
   -- Put_U_Name --
   ----------------

   procedure Put_U_Name (N : Name_Id) is
   begin
      Put (To_Upper (Get_Name_String (N)));
   end Put_U_Name;

   ------------------
   -- Put_Variable --
   ------------------

   procedure Put_Variable
     (Project : Project_Node_Id;
      Pkg     : Name_Id;
      Name    : Name_Id)
   is
   begin
      Put_U_Name (Name_Of (Project));

      if Pkg /= No_Name then
         Put (".");
         Put_L_Name (Pkg);
      end if;

      Put (".");
      Put_U_Name (Name);
   end Put_Variable;

   -----------------------
   -- Recursive_Process --
   -----------------------

   procedure Recursive_Process (Project : Project_Node_Id) is
      With_Clause        : Project_Node_Id;
      Last_Case          : Natural := Last_Case_Construction;
      There_Are_Cases    : Boolean := False;
      May_Be_C_Sources   : Boolean := False;
      May_Be_Cxx_Sources : Boolean := False;
      Post_Processing    : Boolean := False;
      Src_Files_Init     : IO.Position;
      Src_List_File_Init : IO.Position;
   begin
      --  Nothing to do if Project is nil.

      if Project /= Empty_Node then
         declare
            Declaration_Node : constant Project_Node_Id :=
                                 Project_Declaration_Of (Project);
            --  Used to get the project being extended, if any, and the
            --  declarative items of the project to be processed.

            Name : constant Name_Id := Name_Of (Project);
            --  Name of the project being processed

            Directory  : constant Name_Id := Directory_Of (Project);
            --  Directory of the project being processed. Used as default
            --  for the object directory and the source directories.

            Lname : constant String := To_Lower (Get_Name_String (Name));
            --  <project>: name of the project in lower case

            Uname : constant String := To_Upper (Lname);
            --  <PROJECT>: name of the project in upper case

         begin
            --  Nothing to do if project file has already been processed

            if Processed_Projects.Get (Name) = Empty_Node then

               --  Put project name in table Processed_Projects to avoid
               --  processing the project several times.

               Processed_Projects.Set (Name, Project);

               --  Process all the projects imported, if any

               if Process_All_Project_Files then
                  With_Clause := First_With_Clause_Of (Project);

                  while With_Clause /= Empty_Node loop
                     Recursive_Process (Project_Node_Of (With_Clause));
                     With_Clause := Next_With_Clause_Of (With_Clause);
                  end loop;

                  --  Process the project being extended, if any.
                  --  If there is no project being extended,
                  --  Process_Declarative_Items will be called with Empty_Node
                  --  and nothing will happen.

                  Recursive_Process (Extended_Project_Of (Declaration_Node));
               end if;

               Source_Files_Declaration     := False;
               Source_List_File_Declaration := False;

               --  Build in Name_Buffer the path name of the Makefile

               --  Start with the directory of the project file

               Get_Name_String (Directory);

               --  Add a directory separator, if needed

               if Name_Buffer (Name_Len) /= Directory_Separator then
                  Name_Len := Name_Len + 1;
                  Name_Buffer (Name_Len) := Directory_Separator;
               end if;

               --  Add the filename of the Makefile: "Makefile.<project>"

               Name_Buffer (Name_Len + 1 .. Name_Len + 9) := "Makefile.";
               Name_Len := Name_Len + 9;

               Name_Buffer (Name_Len + 1 .. Name_Len + Lname'Length) :=
                 Lname;
               Name_Len := Name_Len + Lname'Length;

               IO.Create (Name_Buffer (1 .. Name_Len));

               --  Display the Makefile being created, but only if not in
               --  quiet output.

               if not Opt.Quiet_Output then
                  Write_Str ("creating """);
                  Write_Str (IO.Name_Of_File);
                  Write_Line ("""");
               end if;

               --  And create the Makefile

               New_Line;

               --  Outut a comment with the path name of the Makefile
               Put ("# ");
               Put_Line (IO.Name_Of_File);

               New_Line;

               --  The Makefile is a big ifeq to avoid multiple inclusion
               --  ifeq ($(<PROJECT>.project),)
               --  <PROJECT>.project:=True
               --    ...
               --  endif

               Put ("ifeq ($(");
               Put (Uname);
               Put (".project),)");
               New_Line;

               Put (Uname);
               Put (".project=True");
               New_Line;

               New_Line;

               --  If it is the main Makefile (BASE_DIR is empty)

               Put_Line ("ifeq ($(BASE_DIR),)");

               --  Set <PROJECT>.root to True

               Put ("   ");
               Put (Uname);
               Put (".root=True");
               New_Line;

               Put ("   ");
               Put (Uname);
               Put (".base_dir:=$(shell gprcmd pwd)");
               New_Line;

               --  Include some utility functions and saved all reserved
               --  env. vars. by including Makefile.prolog.

               New_Line;

               --  First, if MAKE_ROOT is not defined, try to get GNAT prefix

               Put ("   ifeq ($(");
               Put (MAKE_ROOT);
               Put ("),)");
               New_Line;

               Put ("      MAKE_ROOT=$(shell gprcmd prefix)");
               New_Line;

               Put ("   endif");
               New_Line;

               New_Line;

               --  If MAKE_ROOT is still not defined, then fail

               Put ("   ifeq ($(");
               Put (MAKE_ROOT);
               Put ("),)");
               New_Line;

               Put ("      $(error ");
               Put (MAKE_ROOT);
               Put (" variable is undefined, ");
               Put ("Makefile.prolog cannot be loaded)");
               New_Line;

               Put_Line ("   else");

               Put  ("      include $(");
               Put (MAKE_ROOT);
               Put (")");
               Put_Directory_Separator;
               Put ("share");
               Put_Directory_Separator;
               Put ("gnat");
               Put_Directory_Separator;
               Put ("Makefile.prolog");
               New_Line;

               Put_Line ("   endif");

               --  Initialize some defaults

               Put ("   OBJ_EXT:=");
               Put (Get_Object_Suffix.all);
               New_Line;

               Put_Line ("else");

               --  When not the main Makefile, set <PROJECT>.root to False

               Put ("   ");
               Put (Uname);
               Put (".root=False");
               New_Line;

               Put ("   ");
               Put (Uname);
               Put (".base_dir:=$(BASE_DIR)");
               New_Line;

               Put_Line ("endif");
               New_Line;

               --  For each imported project, if any, set BASE_DIR to the
               --  directory of the imported project, and add an include
               --  directive for the Makefile of the imported project.

               With_Clause := First_With_Clause_Of (Project);

               while With_Clause /= Empty_Node loop
                  Put_Include_Project
                    (String_Value_Of (With_Clause),
                     Project_Node_Of (With_Clause),
                     Uname);
                  With_Clause := Next_With_Clause_Of (With_Clause);
               end loop;

               --  Do the same if there is a project being extended.
               --  If there is no project being extended, Put_Include_Project
               --  will return immediately.

               Put_Include_Project
                 (Extended_Project_Path_Of (Project),
                  Extended_Project_Of (Declaration_Node),
                  Uname);

               --  Set defaults to some variables

               IO.Mark (Src_Files_Init);
               Put_Line ("src_files.specified:=FALSE");

               IO.Mark (Src_List_File_Init);
               Put_Line ("src_list_file.specified:=FALSE");

               --  <PROJECT>.src_dirs is set by default to the project
               --  directory.

               Put (Uname);
               Put (".src_dirs:=$(");
               Put (Uname);
               Put (".base_dir)");
               New_Line;

               --  <PROJECT>.obj_dir is set by default to the project
               --  directory.

               Put (Uname);
               Put (".obj_dir:=$(");
               Put (Uname);
               Put (".base_dir)");
               New_Line;

               --  PROJECT_FILE:=<project>

               Put ("PROJECT_FILE:=");
               Put (Lname);
               New_Line;

               --  Output a comment indicating the name of the project being
               --  processed.

               Put ("# project ");
               Put_M_Name (Name);
               New_Line;

               --  Process the external references of this project file

               Process_Externals (Project);

               New_Line;

               --  Reset the compiler switches, the suffixes and the languages

               Switches.Init;
               Reset_Suffixes_And_Languages;

               --  Record the current value of Last_Case_Construction to
               --  detect if there are case constructions in this project file.

               Last_Case := Last_Case_Construction;

               --  Process the declarative items of this project file

               Process_Declarative_Items
                 (Project => Project,
                  Pkg     => No_Name,
                  In_Case => False,
                  Item    => First_Declarative_Item_Of (Declaration_Node));

               --  Set There_Are_Case to True if there are case constructions
               --  in this project file.

               There_Are_Cases := Last_Case /= Last_Case_Construction;

               --  If the suffixs and the languages have not been specified,
               --  give them the default values.

               if C_Suffix_Static and then C_Suffix_Last = 0 then
                  C_Suffix_Last := 2;
                  C_Suffix (1 .. 2) := ".c";
               end if;

               if Cxx_Suffix_Static and then Cxx_Suffix_Last = 0 then
                  Cxx_Suffix_Last := 3;
                  Cxx_Suffix (1 .. 3) := ".cc";
               end if;

               if Ada_Body_Suffix_Static and then Ada_Body_Suffix_Last = 0 then
                  Ada_Body_Suffix_Last := 4;
                  Ada_Body_Suffix (1 .. 4) := ".adb";
               end if;

               if Ada_Spec_Suffix_Static and then Ada_Spec_Suffix_Last = 0 then
                  Ada_Spec_Suffix_Last := 4;
                  Ada_Spec_Suffix (1 .. 4) := ".ads";
               end if;

               if Languages_Static and then Languages_Last = 0 then
                  Languages_Last := 5;
                  Languages (1 .. 5) := " ada ";
               end if;

               --  There may be C sources if the languages are not known
               --  statically or if the languages include "C".

               May_Be_C_Sources := (not Languages_Static)
                 or else Index
                 (Source => Languages (1 .. Languages_Last),
                  Pattern => " c ") /= 0;

               --  There may be C++ sources if the languages are not known
               --  statically or if the languages include "C++".

               May_Be_Cxx_Sources := (not Languages_Static)
                 or else Index
                 (Source => Languages (1 .. Languages_Last),
                  Pattern => " c++ ") /= 0;

               New_Line;

               --  If there are attribute Switches specified in package
               --  Compiler of this project, post-process them.

               if Switches.Last >= Switches.First then

                  --  Output a comment indicating this post-processing

                  for Index in Switches.First .. Switches.Last loop
                     Get_Name_String (Switches.Table (Index));

                     declare
                        File        : constant String :=
                                        Name_Buffer (1 .. Name_Len);
                        Source_Kind : Source_Kind_Type := Unknown;

                     begin
                        --  First, attempt to determine the language

                        if Ada_Body_Suffix_Static then
                           if File'Length > Ada_Body_Suffix_Last
                             and then
                               File (File'Last - Ada_Body_Suffix_Last + 1 ..
                                       File'Last) =
                                          Ada_Body_Suffix
                                            (1 .. Ada_Body_Suffix_Last)
                           then
                              Source_Kind := Ada_Body;
                           end if;
                        end if;

                        if Source_Kind = Unknown
                          and then Ada_Spec_Suffix_Static
                        then
                           if File'Length > Ada_Spec_Suffix_Last
                             and then
                               File (File'Last - Ada_Spec_Suffix_Last + 1 ..
                                       File'Last) =
                                         Ada_Spec_Suffix
                                           (1 .. Ada_Spec_Suffix_Last)
                           then
                              Source_Kind := Ada_Spec;
                           end if;
                        end if;

                        if Source_Kind = Unknown
                          and then C_Suffix_Static
                        then
                           if File'Length > C_Suffix_Last
                             and then
                               File (File'Last - C_Suffix_Last + 1
                                       .. File'Last) =
                                              C_Suffix (1 .. C_Suffix_Last)
                           then
                              Source_Kind := C;
                           end if;
                        end if;

                        if Source_Kind = Unknown
                          and then Cxx_Suffix_Static
                        then
                           if File'Length > Cxx_Suffix_Last
                             and then
                               File (File'Last - Cxx_Suffix_Last + 1
                                         .. File'Last) =
                                              Cxx_Suffix (1 .. Cxx_Suffix_Last)
                           then
                              Source_Kind := Cxx;
                           end if;
                        end if;

                        --  If we still don't know the language, and all
                        --  suffixs are static, then it cannot any of the
                        --  processed languages.

                        if Source_Kind = Unknown
                          and then Ada_Body_Suffix_Static
                          and then Ada_Spec_Suffix_Static
                          and then C_Suffix_Static
                          and then Cxx_Suffix_Static
                        then
                           Source_Kind := None;
                        end if;

                        --  If it can be "C" or "C++", post-process

                        if (Source_Kind = Unknown and
                              (May_Be_C_Sources or May_Be_Cxx_Sources))
                          or else (May_Be_C_Sources and Source_Kind = C)
                          or else (May_Be_Cxx_Sources and Source_Kind = Cxx)
                        then
                           if not Post_Processing then
                              Post_Processing := True;
                              Put_Line
                                ("# post-processing of Compiler'Switches");
                           end if;

                           New_Line;

                           --  Output a comment:
                           --  # for Switches (<file>) use ...

                           Put ("# for Switches (""");
                           Put (File);
                           Put (""") use ...");
                           New_Line;

                           if There_Are_Cases then

                              --  Check that effectively there was Switches
                              --  specified for this file: the attribute
                              --  declaration may be in a case branch which was
                              --  not followed.

                              Put ("ifneq ($(");
                              Put (Uname);
                              Put (".compiler.switches.");
                              Put (File);
                              Put ("),)");
                              New_Line;
                           end if;

                           if May_Be_C_Sources
                             and then
                             (Source_Kind = Unknown or else Source_Kind = C)
                           then
                              --  If it is definitely a C file, no need to test

                              if Source_Kind = C then
                                 Put (File (1 .. File'Last - C_Suffix_Last));
                                 Put (Get_Object_Suffix.all);
                                 Put (": ");
                                 Put (File);
                                 New_Line;

                              else
                                 --  May be a C file: test to know

                                 Put ("ifeq ($(filter %$(C_EXT),");
                                 Put (File);
                                 Put ("),");
                                 Put (File);
                                 Put (")");
                                 New_Line;

                                 --  If it is, output a rule for the object

                                 Put ("$(subst $(C_EXT),$(OBJ_EXT),");
                                 Put (File);
                                 Put ("): ");
                                 Put (File);
                                 New_Line;
                              end if;

                              Put (ASCII.HT & "@echo $(CC) -c $(");
                              Put (Uname);
                              Put (".compiler.switches.");
                              Put (File);
                              Put (") $< -o $(OBJ_DIR)/$@");
                              New_Line;

                              --  If FAKE_COMPILE is defined, do not issue
                              --  the compile command.

                              Put_Line ("ifndef FAKE_COMPILE");

                              Put (ASCII.HT & "@$(CC) -c $(");
                              Put (Uname);
                              Put (".compiler.switches.");
                              Put (File);
                              Put (") $(C_INCLUDES) $(DEP_CFLAGS) " &
                                     "$< -o $(OBJ_DIR)/$@");
                              New_Line;

                              Put_Line (ASCII.HT & "@$(post-compile)");

                              Put_Line ("endif");

                              if Source_Kind = Unknown then
                                 Put_Line ("endif");
                              end if;
                           end if;

                           --  Now, test if it is a C++ file

                           if May_Be_Cxx_Sources
                             and then
                               (Source_Kind = Unknown
                                  or else
                                Source_Kind = Cxx)
                           then
                              --  No need to test if definitely a C++ file

                              if Source_Kind = Cxx then
                                 Put (File (1 .. File'Last - Cxx_Suffix_Last));
                                 Put (Get_Object_Suffix.all);
                                 Put (": ");
                                 Put (File);
                                 New_Line;

                              else
                                 --  May be a C++ file: test to know

                                 Put ("ifeq ($(filter %$(CXX_EXT),");
                                 Put (File);
                                 Put ("),");
                                 Put (File);
                                 Put (")");
                                 New_Line;

                                 --  If it is, output a rule for the object

                                 Put ("$(subst $(CXX_EXT),$(OBJ_EXT),");
                                 Put (File);
                                 Put ("): $(");
                                 Put (Uname);
                                 Put (".absolute.");
                                 Put (File);
                                 Put (")");
                                 New_Line;
                              end if;

                              Put (ASCII.HT & "@echo $(CXX) -c $(");
                              Put (Uname);
                              Put (".compiler.switches.");
                              Put (File);
                              Put (") $< -o $(OBJ_DIR)/$@");
                              New_Line;

                              --  If FAKE_COMPILE is defined, do not issue
                              --  the compile command

                              Put_Line ("ifndef FAKE_COMPILE");

                              Put (ASCII.HT & "@$(CXX) -c $(");
                              Put (Uname);
                              Put (".compiler.switches.");
                              Put (File);
                              Put (") $(C_INCLUDES) $(DEP_CFLAGS) " &
                                     "$< -o $(OBJ_DIR)/$@");
                              New_Line;

                              Put_Line (ASCII.HT & "@$(post-compile)");

                              Put_Line ("endif");

                              if Source_Kind = Unknown then
                                 Put_Line ("endif");
                              end if;

                           end if;

                           if There_Are_Cases then
                              Put_Line ("endif");
                           end if;

                           New_Line;
                        end if;
                     end;
                  end loop;

                  --  Output a comment indication end of post-processing
                  --  of Switches, if we have done some post-processing

                  if Post_Processing then
                     Put_Line
                       ("# end of post-processing of Compiler'Switches");

                     New_Line;
                  end if;
               end if;

               --  Add source dirs of this project file to variable SRC_DIRS

               Put ("SRC_DIRS:=$(SRC_DIRS) $(");
               Put (Uname);
               Put (".src_dirs)");
               New_Line;

               --  Set OBJ_DIR to the object directory

               Put ("OBJ_DIR:=$(");
               Put (Uname);
               Put (".obj_dir)");
               New_Line;

               New_Line;

               if Source_Files_Declaration = True then

                  --  It is guaranteed that Source_Files has been specified.
                  --  We then suppress the two lines that initialize
                  --  the variables src_files.specified and
                  --  src_list_file.specified. Nothing else to do.

                  IO.Suppress (Src_Files_Init);
                  IO.Suppress (Src_List_File_Init);

               else
                  if Source_Files_Declaration = May_Be then

                     --  Need to test if attribute Source_Files was specified

                     Put_Line ("# get the source files, if necessary");
                     Put_Line ("ifeq ($(src_files.specified),FALSE)");

                  else
                     Put_Line ("# get the source files");

                     --  We may suppress initialization of src_files.specified

                     IO.Suppress (Src_Files_Init);
                  end if;

                  if Source_List_File_Declaration /= May_Be then
                     IO.Suppress (Src_List_File_Init);
                  end if;

                  case Source_List_File_Declaration is

                     --  Source_List_File was specified

                     when True =>
                        if Source_Files_Declaration = May_Be then
                           Put ("   ");
                        end if;

                        Put (Uname);
                        Put (".src_files:= $(shell gprcmd cat " &
                             "$(src.list_file))");
                        New_Line;

                     --  Source_File_List was NOT specified

                     when False =>
                        if Source_Files_Declaration = May_Be then
                           Put ("   ");
                        end if;

                        Put (Uname);
                        Put (".src_files:= $(foreach name,$(");
                        Put (Uname);
                        Put (".src_dirs),$(notdir $(wildcard $(name)/*)))");
                        New_Line;

                     when May_Be =>
                        if Source_Files_Declaration = May_Be then
                           Put ("   ");
                        end if;

                        Put_Line ("ifeq ($(src_list_file.specified),TRUE)");

                        --  Get the source files from the file

                        if Source_Files_Declaration = May_Be then
                           Put ("   ");
                        end if;

                        Put ("   ");
                        Put (Uname);
                        Put (".src_files:= $(shell gprcmd cat " &
                             "$(SRC__$LIST_FILE))");
                        New_Line;

                        if Source_Files_Declaration = May_Be then
                           Put ("   ");
                        end if;

                        Put_Line ("else");

                        --  Otherwise get source from the source directories

                        if Source_Files_Declaration = May_Be then
                           Put ("   ");
                        end if;

                        Put ("   ");
                        Put (Uname);
                        Put (".src_files:= $(foreach name,$(");
                        Put (Uname);
                        Put (".src_dirs),$(notdir $(wildcard $(name)/*)))");
                        New_Line;

                        if Source_Files_Declaration = May_Be then
                           Put ("   ");
                        end if;

                        Put_Line ("endif");
                  end case;

                  if Source_Files_Declaration = May_Be then
                     Put_Line ("endif");
                  end if;

                  New_Line;
               end if;

               if not Languages_Static then

                  --  If Languages include "c", get the C sources

                  Put_Line
                    ("# get the C source files, if C is one of the languages");

                  Put_Line ("ifeq ($(filter c,$(LANGUAGES)),c)");

                  Put ("   C_SRCS:=$(filter %$(C_EXT),$(");
                  Put (Uname);
                  Put (".src_files))");
                  New_Line;
                  Put_Line ("   C_SRCS_DEFINED:=True");

                  --  Otherwise set C_SRCS to empty

                  Put_Line ("else");
                  Put_Line ("   C_SRCS=");
                  Put_Line ("endif");
                  New_Line;

                  --  If Languages include "C++", get the C++ sources

                  Put_Line
                    ("# get the C++ source files, " &
                       "if C++ is one of the languages");

                  Put_Line ("ifeq ($(filter c++,$(LANGUAGES)),c++)");

                  Put ("   CXX_SRCS:=$(filter %$(CXX_EXT),$(");
                  Put (Uname);
                  Put (".src_files))");
                  New_Line;
                  Put_Line ("   CXX_SRCS_DEFINED:=True");

                  --  Otherwise set CXX_SRCS to empty

                  Put_Line ("else");
                  Put_Line ("   CXX_SRCS=");
                  Put_Line ("endif");
                  New_Line;

               else
                  if Ada.Strings.Fixed.Index
                    (Languages (1 .. Languages_Last), " c ") /= 0
                  then
                     Put_Line ("# get the C sources");
                     Put ("C_SRCS:=$(filter %$(C_EXT),$(");
                     Put (Uname);
                     Put (".src_files))");
                     New_Line;
                     Put_Line ("C_SRCS_DEFINED:=True");

                  else
                     Put_Line ("# no C sources");

                     Put_Line ("C_SRCS=");
                  end if;

                  New_Line;

                  if Ada.Strings.Fixed.Index
                    (Languages (1 .. Languages_Last), " c++ ") /= 0
                  then
                     Put_Line ("# get the C++ sources");
                     Put ("CXX_SRCS:=$(filter %$(CXX_EXT),$(");
                     Put (Uname);
                     Put (".src_files))");
                     New_Line;
                     Put_Line ("CXX_SRCS_DEFINED:=True");

                  else
                     Put_Line ("# no C++ sources");

                     Put_Line ("CXX_SRCS=");
                  end if;

                  New_Line;
               end if;

               declare
                  C_Present : constant Boolean :=
                                (not Languages_Static) or else
                                Ada.Strings.Fixed.Index
                                  (Languages (1 .. Languages_Last), " c ")
                                   /= 0;

                  Cxx_Present : constant Boolean :=
                                  (not Languages_Static) or else
                                  Ada.Strings.Fixed.Index
                                    (Languages (1 .. Languages_Last), " c++ ")
                                     /= 0;

               begin
                  if C_Present or Cxx_Present then

                     --  If there are C or C++ sources,
                     --  add a library name to variable LIBS.

                     Put ("# if there are ");

                     if C_Present then
                        if Cxx_Present then
                           Put ("C or C++");

                        else
                           Put ("C");
                        end if;

                     else
                        Put ("C++");
                     end if;

                     Put (" sources, add the library");
                     New_Line;

                     Put ("ifneq ($(strip");

                     if C_Present then
                        Put (" $(C_SRCS)");
                     end if;

                     if Cxx_Present then
                        Put (" $(CXX_SRCS)");
                     end if;

                     Put ("),)");
                     New_Line;

                     Put ("   LIBS:=$(");
                     Put (Uname);
                     Put (".obj_dir)/lib");
                     Put (Lname);
                     Put ("$(AR_EXT) $(LIBS)");
                     New_Line;

                     Put_Line ("endif");

                     New_Line;

                  end if;
               end;

               --  If this is the main Makefile, include Makefile.Generic

               Put ("ifeq ($(");
               Put (Uname);
               Put_Line (".root),True)");

               --  Include Makefile.generic

               Put ("   include $(");
               Put (MAKE_ROOT);
               Put (")");
               Put_Directory_Separator;
               Put ("share");
               Put_Directory_Separator;
               Put ("gnat");
               Put_Directory_Separator;
               Put ("Makefile.generic");
               New_Line;

               --  If it is not the main Makefile, add the project to
               --  variable DEPS_PROJECTS.

               Put_Line ("else");

               Put ("   DEPS_PROJECTS:=$(strip $(DEPS_PROJECTS) $(");
               Put (Uname);
               Put (".base_dir)/");
               Put (Lname);
               Put (")");
               New_Line;

               Put_Line ("endif");
               New_Line;

               Put_Line ("endif");
               New_Line;

               --  Close the Makefile, so that another Makefile can be created
               --  with the same File_Type variable.

               IO.Close;
            end if;
         end;
      end if;
   end Recursive_Process;

   ----------------------------------
   -- Reset_Suffixes_And_Languages --
   ----------------------------------

   procedure Reset_Suffixes_And_Languages is
   begin
      --  Last = 0 indicates that this is the default, which is static,
      --  of course.

      C_Suffix_Last           := 0;
      C_Suffix_Static         := True;
      Cxx_Suffix_Last         := 0;
      Cxx_Suffix_Static       := True;
      Ada_Body_Suffix_Last    := 0;
      Ada_Body_Suffix_Static  := True;
      Ada_Spec_Suffix_Last    := 0;
      Ada_Spec_Suffix_Static  := True;
      Languages_Last          := 0;
      Languages_Static        := True;
   end Reset_Suffixes_And_Languages;

   --------------------
   -- Source_Kind_Of --
   --------------------

   function Source_Kind_Of (File_Name : Name_Id) return Source_Kind_Type is
      Source_C_Suffix   : constant String :=
        Suffix_Of (C_Suffix_Static, C_Suffix, C_Suffix_Last, ".c");

      Source_Cxx_Suffix : constant String :=
        Suffix_Of (Cxx_Suffix_Static, Cxx_Suffix, Cxx_Suffix_Last, ".cc");

      Body_Ada_Suffix   : constant String :=
        Suffix_Of
          (Ada_Body_Suffix_Static,
           Ada_Body_Suffix,
           Ada_Body_Suffix_Last,
           ".adb");

      Spec_Ada_Suffix   : constant String :=
        Suffix_Of
          (Ada_Spec_Suffix_Static,
           Ada_Spec_Suffix,
           Ada_Spec_Suffix_Last,
           ".ads");

   begin
      --  Get the name of the file

      Get_Name_String (File_Name);

      --  If the C suffix is static, check if it is a C file

      if Source_C_Suffix /= ""
        and then Name_Len > Source_C_Suffix'Length
        and then Name_Buffer (Name_Len - Source_C_Suffix'Length + 1
                                .. Name_Len) = Source_C_Suffix
      then
         return C;

      --  If the C++ suffix is static, check if it is a C++ file

      elsif Source_Cxx_Suffix /= ""
        and then Name_Len > Source_Cxx_Suffix'Length
        and then Name_Buffer (Name_Len - Source_Cxx_Suffix'Length + 1
                                .. Name_Len) = Source_Cxx_Suffix
      then
         return Cxx;

      --  If the Ada body suffix is static, check if it is an Ada body

      elsif Body_Ada_Suffix /= ""
        and then Name_Len > Body_Ada_Suffix'Length
        and then Name_Buffer (Name_Len - Body_Ada_Suffix'Length + 1
                                .. Name_Len) = Body_Ada_Suffix
      then
         return Ada_Body;

      --  If the Ada spec suffix is static, check if it is an Ada spec

      elsif Spec_Ada_Suffix /= ""
        and then Name_Len > Spec_Ada_Suffix'Length
        and then Name_Buffer (Name_Len - Spec_Ada_Suffix'Length + 1
                                .. Name_Len) = Spec_Ada_Suffix
      then
         return Ada_Body;

      --  If the C or C++ suffix is not static, then return Unknown

      elsif Source_C_Suffix = "" or else Source_Cxx_Suffix = "" then
         return Unknown;

      --  Otherwise return None

      else
         return None;
      end if;
   end Source_Kind_Of;

   ------------------------
   -- Special_Put_U_Name --
   ------------------------

   procedure Special_Put_U_Name (S : Name_Id) is
   begin
      Get_Name_String (S);
      To_Upper (Name_Buffer (1 .. Name_Len));

      --  If string is "C++", change it to "CXX"

      if Name_Buffer (1 .. Name_Len) = "C++" then
         Put ("CXX");
      else
         Put (Name_Buffer (1 .. Name_Len));
      end if;
   end Special_Put_U_Name;

   ---------------
   -- Suffix_Of --
   ---------------

   function Suffix_Of
     (Static  : Boolean;
      Value   : String_Access;
      Last    : Natural;
      Default : String) return String
   is
   begin
      if Static then

         --  If the suffix is static, Last = 0 indicates that it is the default
         --  suffix: return the default.

         if Last = 0 then
            return Default;

         --  Otherwise, return the current suffix

         else
            return Value (1 .. Last);
         end if;

      --  If the suffix is not static, return ""

      else
         return "";
      end if;
   end Suffix_Of;

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      if not Usage_Displayed then
         Usage_Displayed := True;
         Display_Copyright;
         Write_Line ("Usage: gpr2make switches project-file");
         Write_Eol;
         Write_Line ("   -h   Display this usage");
         Write_Line ("   -q   Quiet output");
         Write_Line ("   -v   Verbose mode");
         Write_Line ("   -R   not Recursive: only one project file");
         Write_Eol;
      end if;
   end Usage;
end Bld;
