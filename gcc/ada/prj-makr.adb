------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P R J . M A K R                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2001-2003 Free Software Foundation, Inc.          --
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

with Csets;
with Namet;    use Namet;
with Opt;
with Output;
with Osint;    use Osint;
with Prj;      use Prj;
with Prj.Com;
with Prj.Part;
with Prj.PP;
with Prj.Tree; use Prj.Tree;
with Snames;   use Snames;
with Table;    use Table;

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Expect;               use GNAT.Expect;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Regexp;               use GNAT.Regexp;
with GNAT.Regpat;               use GNAT.Regpat;

package body Prj.Makr is

   Non_Empty_Node : constant Project_Node_Id := 1;
   --  Used for the With_Clause of the naming project

   type Matched_Type is (True, False, Excluded);

   Naming_File_Suffix      : constant String := "_naming";
   Source_List_File_Suffix : constant String := "_source_list.txt";

   Output_FD   : File_Descriptor;
   --  To save the project file and its naming project file.

   procedure Write_Eol;
   --  Output an empty line.

   procedure Write_A_Char (C : Character);
   --  Write one character to Output_FD

   procedure Write_A_String (S : String);
   --  Write a String to Output_FD

   package Processed_Directories is new Table.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 0,
      Table_Initial        => 10,
      Table_Increment      => 10,
      Table_Name           => "Prj.Makr.Processed_Directories");

   ----------
   -- Make --
   ----------

   procedure Make
     (File_Path         : String;
      Project_File      : Boolean;
      Directories       : Argument_List;
      Name_Patterns     : Argument_List;
      Excluded_Patterns : Argument_List;
      Foreign_Patterns  : Argument_List;
      Preproc_Switches  : Argument_List;
      Very_Verbose      : Boolean)
   is
      Path_Name : String (1 .. File_Path'Length +
                            Project_File_Extension'Length);
      Path_Last : Natural := File_Path'Length;

      Directory_Last    : Natural := 0;

      Output_Name      : String (Path_Name'Range);
      Output_Name_Last : Natural;
      Output_Name_Id   : Name_Id;

      Project_Node        : Project_Node_Id := Empty_Node;
      Project_Declaration : Project_Node_Id := Empty_Node;
      Source_Dirs_List    : Project_Node_Id := Empty_Node;
      Current_Source_Dir  : Project_Node_Id := Empty_Node;

      Project_Naming_Node : Project_Node_Id := Empty_Node;
      Project_Naming_Decl : Project_Node_Id := Empty_Node;
      Naming_Package      : Project_Node_Id := Empty_Node;

      Project_Naming_File_Name : String (1 .. Output_Name'Length +
                                           Naming_File_Suffix'Length);

      Project_Naming_Last : Natural;
      Project_Naming_Id   : Name_Id := No_Name;

      Excluded_Expressions : array (Excluded_Patterns'Range) of Regexp;
      Regular_Expressions  : array (Name_Patterns'Range) of Regexp;
      Foreign_Expressions  : array (Foreign_Patterns'Range) of Regexp;

      Source_List_Path : String (1 .. Output_Name'Length +
                                   Source_List_File_Suffix'Length);
      Source_List_Last : Natural;

      Source_List_FD : File_Descriptor;

      Matcher : constant Pattern_Matcher :=
                  Compile (Expression => "expected|Unit.*\)|No such");

      Args : Argument_List  (1 .. Preproc_Switches'Length + 6);
--                 (1 => new String'("-c"),
--                  2 => new String'("-gnats"),
--                  3 => new String'("-gnatu"),
--                  4 => new String'("-x"),
--                  5 => new String'("ada"),
--                  6 => null);

      type SFN_Pragma is record
         Unit : String_Access;
         File : String_Access;
         Spec : Boolean;
      end record;

      package SFN_Pragmas is new Table.Table
        (Table_Component_Type => SFN_Pragma,
         Table_Index_Type     => Natural,
         Table_Low_Bound      => 0,
         Table_Initial        => 50,
         Table_Increment      => 50,
         Table_Name           => "Prj.Makr.SFN_Pragmas");

      procedure Process_Directory (Dir_Name : String; Recursively : Boolean);
      --  Look for Ada and foreign sources in a directory, according to the
      --  patterns. When Recursively is True, after looking for sources in
      --  Dir_Name, look also in its subdirectories, if any.

      -----------------------
      -- Process_Directory --
      -----------------------

      procedure Process_Directory (Dir_Name : String; Recursively : Boolean) is
         Matched  : Matched_Type := False;
         Str      : String (1 .. 2_000);
         Last     : Natural;
         Dir      : Dir_Type;
         Process  : Boolean := True;

      begin
         if Opt.Verbose_Mode then
            Output.Write_Str ("Processing directory """);
            Output.Write_Str (Dir_Name);
            Output.Write_Line ("""");
         end if;

         --  Avoid processing several times the same directory.

         for Index in 1 .. Processed_Directories.Last loop
            if Processed_Directories.Table (Index).all = Dir_Name then
               Process := False;
               exit;
            end if;
         end loop;

         if Process then
            Processed_Directories. Increment_Last;
            Processed_Directories.Table (Processed_Directories.Last) :=
              new String'(Dir_Name);
            --  Get the source file names from the directory.
            --  Fails if the directory does not exist.

            begin
               Open (Dir, Dir_Name);

            exception
               when Directory_Error =>
                  Prj.Com.Fail ("cannot open directory """, Dir_Name, """");
            end;

            --  Process each regular file in the directory

            loop
               Read (Dir, Str, Last);
               exit when Last = 0;

               if Is_Regular_File
                 (Dir_Name & Directory_Separator & Str (1 .. Last))
               then
                  Matched := True;

                  --  First, check if the file name matches at least one of
                  --  the excluded expressions;

                  for Index in Excluded_Expressions'Range loop
                     if
                       Match (Str (1 .. Last), Excluded_Expressions (Index))
                     then
                        Matched := Excluded;
                        exit;
                     end if;
                  end loop;

                  --  If it does not match any of the excluded expressions,
                  --  check if the file name matches at least one of the
                  --  regular expressions.

                  if Matched = True then
                     Matched := False;

                     for Index in Regular_Expressions'Range loop
                        if
                          Match (Str (1 .. Last), Regular_Expressions (Index))
                        then
                           Matched := True;
                           exit;
                        end if;
                     end loop;
                  end if;

                  if Very_Verbose
                    or else (Matched = True and then Opt.Verbose_Mode)
                  then
                     Output.Write_Str ("   Checking """);
                     Output.Write_Str (Str (1 .. Last));
                     Output.Write_Str (""": ");
                  end if;

                  --  If the file name matches one of the regular expressions,
                  --  parse it to get its unit name.

                  if Matched = True then
                     declare
                        PD     : Process_Descriptor;
                        Result : Expect_Match;

                     begin
                        Args (Args'Last) := new String'
                                                  (Dir_Name &
                                                   Directory_Separator &
                                                   Str (1 .. Last));

                        begin
                           Non_Blocking_Spawn
                             (PD, "gcc", Args, Err_To_Out => True);
                           Expect (PD, Result, Matcher);

                        exception
                           when Process_Died =>
                              if Opt.Verbose_Mode then
                                 Output.Write_Str ("(process died) ");
                              end if;

                              Result := Expect_Timeout;
                        end;

                        if Result /= Expect_Timeout then

                           --  If we got a unit name, this is a valid source
                           --  file.

                           declare
                              S : constant String := Expect_Out_Match (PD);

                           begin
                              if S'Length >= 13
                                and then S (S'First .. S'First + 3) = "Unit"
                              then
                                 if Opt.Verbose_Mode then
                                    Output.Write_Str
                                      (S (S'Last - 4 .. S'Last - 1));
                                    Output.Write_Str (" of ");
                                    Output.Write_Line
                                      (S (S'First + 5 .. S'Last - 7));
                                 end if;

                                 if Project_File then

                                    --  Add the corresponding attribute in the
                                    --  Naming package of the naming project.

                                    declare
                                       Decl_Item : constant Project_Node_Id :=
                                         Default_Project_Node
                                         (Of_Kind =>
                                            N_Declarative_Item);

                                       Attribute : constant Project_Node_Id :=
                                         Default_Project_Node
                                         (Of_Kind =>
                                            N_Attribute_Declaration);

                                       Expression : constant Project_Node_Id :=
                                         Default_Project_Node
                                         (Of_Kind => N_Expression,
                                          And_Expr_Kind => Single);

                                       Term : constant Project_Node_Id :=
                                         Default_Project_Node
                                         (Of_Kind => N_Term,
                                          And_Expr_Kind => Single);

                                       Value : constant Project_Node_Id :=
                                         Default_Project_Node
                                         (Of_Kind => N_Literal_String,
                                          And_Expr_Kind => Single);

                                    begin
                                       Set_Next_Declarative_Item
                                         (Decl_Item,
                                          To => First_Declarative_Item_Of
                                            (Naming_Package));
                                       Set_First_Declarative_Item_Of
                                         (Naming_Package, To => Decl_Item);
                                       Set_Current_Item_Node
                                         (Decl_Item, To => Attribute);

                                       if
                                         S (S'Last - 5 .. S'Last) = "(spec)"
                                       then
                                          Set_Name_Of
                                            (Attribute, To => Name_Spec);
                                       else
                                          Set_Name_Of
                                            (Attribute,
                                             To => Name_Body);
                                       end if;

                                       Name_Len := S'Last - S'First - 11;
                                       Name_Buffer (1 .. Name_Len) :=
                                         (To_Lower
                                            (S (S'First + 5 .. S'Last - 7)));
                                       Set_Associative_Array_Index_Of
                                         (Attribute, To => Name_Find);

                                       Set_Expression_Of
                                         (Attribute, To => Expression);
                                       Set_First_Term (Expression, To => Term);
                                       Set_Current_Term (Term, To => Value);

                                       Name_Len := Last;
                                       Name_Buffer (1 .. Name_Len) :=
                                         Str (1 .. Last);
                                       Set_String_Value_Of
                                         (Value, To => Name_Find);
                                    end;

                                    --  Add source file name to source list
                                    --  file.

                                    Last := Last + 1;
                                    Str (Last) := ASCII.LF;

                                    if Write (Source_List_FD,
                                              Str (1)'Address,
                                              Last) /= Last
                                    then
                                       Prj.Com.Fail ("disk full");
                                    end if;
                                 else
                                    --  Add an entry in the SFN_Pragmas table

                                    SFN_Pragmas.Increment_Last;
                                    SFN_Pragmas.Table (SFN_Pragmas.Last) :=
                                      (Unit => new String'
                                         (S (S'First + 5 .. S'Last - 7)),
                                       File => new String'(Str (1 .. Last)),
                                       Spec => S (S'Last - 5 .. S'Last)
                                       = "(spec)");
                                 end if;

                              else
                                 if Opt.Verbose_Mode then
                                    Output.Write_Line ("not a unit");
                                 end if;
                              end if;
                           end;

                        else
                           if Opt.Verbose_Mode then
                              Output.Write_Line ("not a unit");
                           end if;
                        end if;

                        Close (PD);
                     end;

                  else
                     if Matched = False then
                        --  Look if this is a foreign source

                        for Index in Foreign_Expressions'Range loop
                           if Match (Str (1 .. Last),
                                     Foreign_Expressions (Index))
                           then
                              Matched := True;
                              exit;
                           end if;
                        end loop;
                     end if;

                     if Very_Verbose then
                        case Matched is
                           when False =>
                              Output.Write_Line ("no match");

                           when Excluded =>
                              Output.Write_Line ("excluded");

                           when True =>
                              Output.Write_Line ("foreign source");
                        end case;
                     end if;

                     if Project_File and Matched = True then

                        --  Add source file name to source list file

                        Last := Last + 1;
                        Str (Last) := ASCII.LF;

                        if Write (Source_List_FD,
                                  Str (1)'Address,
                                  Last) /= Last
                        then
                           Prj.Com.Fail ("disk full");
                        end if;
                     end if;
                  end if;
               end if;
            end loop;

            Close (Dir);
         end if;

         --  If Recursively is True, call itself for each subdirectory.
         --  We do that, even when this directory has already been processed,
         --  because all of its subdirectories may not have been processed.

         if Recursively then
            Open (Dir, Dir_Name);

            loop
               Read (Dir, Str, Last);
               exit when Last = 0;

               --  Do not call itself for "." or ".."

               if Is_Directory
                 (Dir_Name & Directory_Separator & Str (1 .. Last))
                 and then Str (1 .. Last) /= "."
                 and then Str (1 .. Last) /= ".."
               then
                  Process_Directory
                    (Dir_Name & Directory_Separator & Str (1 .. Last),
                     Recursively => True);
               end if;
            end loop;

            Close (Dir);
         end if;
      end Process_Directory;

   --  Start of processing for Make

   begin
      --  Do some needed initializations

      Csets.Initialize;
      Namet.Initialize;
      Snames.Initialize;
      Prj.Initialize;

      SFN_Pragmas.Set_Last (0);

      Processed_Directories.Set_Last (0);

      --  Initialize the compiler switches

      Args (1) := new String'("-c");
      Args (2) := new String'("-gnats");
      Args (3) := new String'("-gnatu");
      Args (4 .. 3 + Preproc_Switches'Length) := Preproc_Switches;
      Args (4 + Preproc_Switches'Length) := new String'("-x");
      Args (5 + Preproc_Switches'Length) := new String'("ada");

      --  Get the path and file names

      if File_Names_Case_Sensitive then
         Path_Name (1 .. Path_Last) := File_Path;
      else
         Path_Name (1 .. Path_Last) := To_Lower (File_Path);
      end if;

      Path_Name (Path_Last + 1 .. Path_Name'Last) :=
        Project_File_Extension;

      --  Get the end of directory information, if any

      for Index in reverse 1 .. Path_Last loop
         if Path_Name (Index) = Directory_Separator then
            Directory_Last := Index;
            exit;
         end if;
      end loop;

      if Project_File then
         if Path_Last < Project_File_Extension'Length + 1
           or else Path_Name
           (Path_Last - Project_File_Extension'Length + 1 .. Path_Last)
           /= Project_File_Extension
         then
            Path_Last := Path_Name'Last;
         end if;

         Output_Name (1 .. Path_Last) := To_Lower (Path_Name (1 .. Path_Last));
         Output_Name_Last := Path_Last - Project_File_Extension'Length;

         if Directory_Last /= 0 then
            Output_Name (1 .. Output_Name_Last - Directory_Last) :=
              Output_Name (Directory_Last + 1 .. Output_Name_Last);
            Output_Name_Last := Output_Name_Last - Directory_Last;
         end if;

         --  Get the project name id

         Name_Len := Output_Name_Last;
         Name_Buffer (1 .. Name_Len) := Output_Name (1 .. Name_Len);
         Output_Name_Id := Name_Find;

         --  Create the project naming file name

         Project_Naming_Last := Output_Name_Last;
         Project_Naming_File_Name (1 .. Project_Naming_Last) :=
           Output_Name (1 .. Project_Naming_Last);
         Project_Naming_File_Name
           (Project_Naming_Last + 1 ..
              Project_Naming_Last + Naming_File_Suffix'Length) :=
           Naming_File_Suffix;
         Project_Naming_Last :=
           Project_Naming_Last + Naming_File_Suffix'Length;

         --  Get the project naming id

         Name_Len := Project_Naming_Last;
         Name_Buffer (1 .. Name_Len) :=
           Project_Naming_File_Name (1 .. Name_Len);
         Project_Naming_Id := Name_Find;

         Project_Naming_File_Name
           (Project_Naming_Last + 1 ..
              Project_Naming_Last + Project_File_Extension'Length) :=
           Project_File_Extension;
         Project_Naming_Last :=
           Project_Naming_Last + Project_File_Extension'Length;

         --  Create the source list file name

         Source_List_Last := Output_Name_Last;
         Source_List_Path (1 .. Source_List_Last) :=
           Output_Name (1 .. Source_List_Last);
         Source_List_Path
           (Source_List_Last + 1 ..
              Source_List_Last + Source_List_File_Suffix'Length) :=
           Source_List_File_Suffix;
         Source_List_Last := Source_List_Last + Source_List_File_Suffix'Length;

         --  Add the project file extension to the project name

         Output_Name
           (Output_Name_Last + 1 ..
              Output_Name_Last + Project_File_Extension'Length) :=
           Project_File_Extension;
         Output_Name_Last := Output_Name_Last + Project_File_Extension'Length;
      end if;

      --  Change the current directory to the directory of the project file,
      --  if any directory information is specified.

      if Directory_Last /= 0 then
         begin
            Change_Dir (Path_Name (1 .. Directory_Last));
         exception
            when Directory_Error =>
               Prj.Com.Fail
                 ("unknown directory """,
                  Path_Name (1 .. Directory_Last),
                  """");
         end;
      end if;

      if Project_File then

         --  Delete the source list file, if it already exists

         declare
            Discard : Boolean;

         begin
            Delete_File
              (Source_List_Path (1 .. Source_List_Last),
               Success => Discard);
         end;

         --  And create a new source list file.
         --  Fail if file cannot be created.

         Source_List_FD := Create_New_File
           (Name  => Source_List_Path (1 .. Source_List_Last),
            Fmode => Text);

         if Source_List_FD = Invalid_FD then
            Prj.Com.Fail
              ("cannot create file """,
               Source_List_Path (1 .. Source_List_Last),
               """");
         end if;
      end if;

      --  Compile the regular expressions. Fails immediately if any of
      --  the specified strings is in error.

      for Index in Excluded_Expressions'Range loop
         if Very_Verbose then
            Output.Write_Str ("Excluded pattern: """);
            Output.Write_Str (Excluded_Patterns (Index).all);
            Output.Write_Line ("""");
         end if;

         begin
            Excluded_Expressions (Index) :=
              Compile (Pattern => Excluded_Patterns (Index).all, Glob => True);

         exception
            when Error_In_Regexp =>
               Prj.Com.Fail
                 ("invalid regular expression """,
                  Excluded_Patterns (Index).all,
                  """");
         end;
      end loop;

      for Index in Foreign_Expressions'Range loop
         if Very_Verbose then
            Output.Write_Str ("Foreign pattern: """);
            Output.Write_Str (Foreign_Patterns (Index).all);
            Output.Write_Line ("""");
         end if;

         begin
            Foreign_Expressions (Index) :=
              Compile (Pattern => Foreign_Patterns (Index).all, Glob => True);

         exception
            when Error_In_Regexp =>
               Prj.Com.Fail
                 ("invalid regular expression """,
                  Foreign_Patterns (Index).all,
                  """");
         end;
      end loop;

      for Index in Regular_Expressions'Range loop
         if Very_Verbose then
            Output.Write_Str ("Pattern: """);
            Output.Write_Str (Name_Patterns (Index).all);
            Output.Write_Line ("""");
         end if;

         begin
            Regular_Expressions (Index) :=
              Compile (Pattern => Name_Patterns (Index).all, Glob => True);

         exception
            when Error_In_Regexp =>
               Prj.Com.Fail
                 ("invalid regular expression """,
                  Name_Patterns (Index).all,
                  """");
         end;
      end loop;

      if Project_File then
         if Opt.Verbose_Mode then
            Output.Write_Str ("Naming project file name is """);
            Output.Write_Str
              (Project_Naming_File_Name (1 .. Project_Naming_Last));
            Output.Write_Line ("""");
         end if;

         --  If there is already a project file with the specified name,
         --  parse it to get the components that are not automatically
         --  generated.

         if Is_Regular_File (Output_Name (1 .. Output_Name_Last)) then
            if Opt.Verbose_Mode then
               Output.Write_Str ("Parsing already existing project file """);
               Output.Write_Str (Output_Name (1 .. Output_Name_Last));
               Output.Write_Line ("""");
            end if;

            Part.Parse
              (Project           => Project_Node,
               Project_File_Name => Output_Name (1 .. Output_Name_Last),
               Always_Errout_Finalize => False);

            --  If parsing was successful, remove the components that are
            --  automatically generated, if any, so that they will be
            --  unconditionally added later.

            if Project_Node /= Empty_Node then

               --  Remove the with clause for the naming project file

               declare
                  With_Clause : Project_Node_Id :=
                    First_With_Clause_Of (Project_Node);
                  Previous    : Project_Node_Id := Empty_Node;

               begin
                  while With_Clause /= Empty_Node loop
                     if Tree.Name_Of (With_Clause) = Project_Naming_Id then
                        if Previous = Empty_Node then
                           Set_First_With_Clause_Of
                             (Project_Node,
                              To => Next_With_Clause_Of (With_Clause));
                        else
                           Set_Next_With_Clause_Of
                             (Previous,
                              To => Next_With_Clause_Of (With_Clause));
                        end if;

                        exit;
                     end if;

                     Previous := With_Clause;
                     With_Clause := Next_With_Clause_Of (With_Clause);
                  end loop;
               end;

               --  Remove attribute declarations of Source_Files,
               --  Source_List_File, Source_Dirs, and the declaration of
               --  package Naming, if they exist.

               declare
                  Declaration  : Project_Node_Id :=
                                   First_Declarative_Item_Of
                                     (Project_Declaration_Of
                                       (Project_Node));
                  Previous     : Project_Node_Id := Empty_Node;
                  Current_Node : Project_Node_Id := Empty_Node;

               begin
                  while Declaration /= Empty_Node loop
                     Current_Node := Current_Item_Node (Declaration);

                     if (Kind_Of (Current_Node) = N_Attribute_Declaration
                           and then
                           (Tree.Name_Of (Current_Node) = Name_Source_Files
                             or else Tree.Name_Of (Current_Node) =
                                               Name_Source_List_File
                              or else Tree.Name_Of (Current_Node) =
                              Name_Source_Dirs))
                       or else
                       (Kind_Of (Current_Node) = N_Package_Declaration
                          and then Tree.Name_Of (Current_Node) = Name_Naming)
                     then
                        if Previous = Empty_Node then
                           Set_First_Declarative_Item_Of
                             (Project_Declaration_Of (Project_Node),
                              To => Next_Declarative_Item (Declaration));

                        else
                           Set_Next_Declarative_Item
                             (Previous,
                              To => Next_Declarative_Item (Declaration));
                        end if;

                     else
                        Previous := Declaration;
                     end if;

                     Declaration := Next_Declarative_Item (Declaration);
                  end loop;
               end;
            end if;
         end if;

         --  If there were no already existing project file, or if the parsing
         --  was unsuccessful, create an empty project node with the correct
         --  name and its project declaration node.

         if Project_Node = Empty_Node then
            Project_Node := Default_Project_Node (Of_Kind => N_Project);
            Set_Name_Of (Project_Node, To => Output_Name_Id);
            Set_Project_Declaration_Of
              (Project_Node,
               To => Default_Project_Node (Of_Kind => N_Project_Declaration));

         end if;

         --  Create the naming project node, and add an attribute declaration
         --  for Source_Files as an empty list, to indicate there are no
         --  sources in the naming project.

         Project_Naming_Node := Default_Project_Node (Of_Kind => N_Project);
         Set_Name_Of (Project_Naming_Node, To => Project_Naming_Id);
         Project_Naming_Decl :=
           Default_Project_Node (Of_Kind => N_Project_Declaration);
         Set_Project_Declaration_Of (Project_Naming_Node, Project_Naming_Decl);
         Naming_Package :=
           Default_Project_Node (Of_Kind => N_Package_Declaration);
         Set_Name_Of (Naming_Package, To => Name_Naming);

         declare
            Decl_Item : constant Project_Node_Id :=
              Default_Project_Node (Of_Kind => N_Declarative_Item);

            Attribute : constant Project_Node_Id :=
              Default_Project_Node
              (Of_Kind => N_Attribute_Declaration,
               And_Expr_Kind => List);

            Expression : constant Project_Node_Id :=
              Default_Project_Node
              (Of_Kind => N_Expression,
               And_Expr_Kind => List);

            Term  : constant Project_Node_Id :=
              Default_Project_Node
              (Of_Kind => N_Term,
               And_Expr_Kind => List);

            Empty_List : constant Project_Node_Id :=
              Default_Project_Node
              (Of_Kind => N_Literal_String_List);

         begin
            Set_First_Declarative_Item_Of
              (Project_Naming_Decl, To => Decl_Item);
            Set_Next_Declarative_Item (Decl_Item, Naming_Package);
            Set_Current_Item_Node (Decl_Item, To => Attribute);
            Set_Name_Of (Attribute, To => Name_Source_Files);
            Set_Expression_Of (Attribute, To => Expression);
            Set_First_Term (Expression, To => Term);
            Set_Current_Term (Term, To => Empty_List);
         end;

         --  Add a with clause on the naming project in the main project

         declare
            With_Clause : constant Project_Node_Id :=
              Default_Project_Node (Of_Kind => N_With_Clause);

         begin
            Set_Next_With_Clause_Of
              (With_Clause, To => First_With_Clause_Of (Project_Node));
            Set_First_With_Clause_Of (Project_Node, To => With_Clause);
            Set_Name_Of (With_Clause, To => Project_Naming_Id);

            --  We set the project node to something different than
            --  Empty_Node, so that Prj.PP does not generate a limited
            --  with clause.

            Set_Project_Node_Of (With_Clause, Non_Empty_Node);

            Name_Len := Project_Naming_Last;
            Name_Buffer (1 .. Name_Len) :=
              Project_Naming_File_Name (1 .. Project_Naming_Last);
            Set_String_Value_Of (With_Clause, To => Name_Find);
         end;

         Project_Declaration := Project_Declaration_Of (Project_Node);

         --  Add a renaming declaration for package Naming in the main project

         declare
            Decl_Item  : constant Project_Node_Id :=
              Default_Project_Node (Of_Kind => N_Declarative_Item);

            Naming : constant Project_Node_Id :=
              Default_Project_Node (Of_Kind => N_Package_Declaration);
         begin
            Set_Next_Declarative_Item
              (Decl_Item,
               To => First_Declarative_Item_Of (Project_Declaration));
            Set_First_Declarative_Item_Of
              (Project_Declaration, To => Decl_Item);
            Set_Current_Item_Node (Decl_Item, To => Naming);
            Set_Name_Of (Naming, To => Name_Naming);
            Set_Project_Of_Renamed_Package_Of
              (Naming, To => Project_Naming_Node);
         end;

         --  Add an attribute declaration for Source_Dirs, initialized as an
         --  empty list. Directories will be added as they are read from the
         --  directory list file.

         declare
            Decl_Item  : constant Project_Node_Id :=
              Default_Project_Node (Of_Kind => N_Declarative_Item);

            Attribute : constant Project_Node_Id :=
              Default_Project_Node
              (Of_Kind => N_Attribute_Declaration,
               And_Expr_Kind => List);

            Expression : constant Project_Node_Id :=
              Default_Project_Node
              (Of_Kind => N_Expression,
               And_Expr_Kind => List);

            Term  : constant Project_Node_Id :=
              Default_Project_Node
              (Of_Kind => N_Term, And_Expr_Kind => List);

         begin
            Set_Next_Declarative_Item
              (Decl_Item,
               To => First_Declarative_Item_Of (Project_Declaration));
            Set_First_Declarative_Item_Of
              (Project_Declaration, To => Decl_Item);
            Set_Current_Item_Node (Decl_Item, To => Attribute);
            Set_Name_Of (Attribute, To => Name_Source_Dirs);
            Set_Expression_Of (Attribute, To => Expression);
            Set_First_Term (Expression, To => Term);
            Source_Dirs_List :=
              Default_Project_Node (Of_Kind => N_Literal_String_List,
                                    And_Expr_Kind => List);
            Set_Current_Term (Term, To => Source_Dirs_List);
         end;

         --  Add an attribute declaration for Source_List_File with the
         --  source list file name that will be created.

         declare
            Decl_Item  : constant Project_Node_Id :=
              Default_Project_Node (Of_Kind => N_Declarative_Item);

            Attribute : constant Project_Node_Id :=
              Default_Project_Node
              (Of_Kind => N_Attribute_Declaration,
               And_Expr_Kind => Single);

            Expression : constant Project_Node_Id :=
              Default_Project_Node
              (Of_Kind => N_Expression,
               And_Expr_Kind => Single);

            Term  : constant Project_Node_Id :=
              Default_Project_Node
              (Of_Kind => N_Term,
               And_Expr_Kind => Single);

            Value : constant Project_Node_Id :=
              Default_Project_Node
              (Of_Kind => N_Literal_String,
               And_Expr_Kind => Single);

         begin
            Set_Next_Declarative_Item
              (Decl_Item,
               To => First_Declarative_Item_Of (Project_Declaration));
            Set_First_Declarative_Item_Of
              (Project_Declaration, To => Decl_Item);
            Set_Current_Item_Node (Decl_Item, To => Attribute);
            Set_Name_Of (Attribute, To => Name_Source_List_File);
            Set_Expression_Of (Attribute, To => Expression);
            Set_First_Term (Expression, To => Term);
            Set_Current_Term (Term, To => Value);
            Name_Len := Source_List_Last;
            Name_Buffer (1 .. Name_Len) :=
              Source_List_Path (1 .. Source_List_Last);
            Set_String_Value_Of (Value, To => Name_Find);
         end;
      end if;

      --  Process each directory

      for Index in Directories'Range  loop

         declare
            Dir_Name    : constant String := Directories (Index).all;
            Last        : Natural := Dir_Name'Last;
            Recursively : Boolean := False;
         begin
            if Dir_Name'Length >= 4
              and then (Dir_Name (Last - 2 .. Last) = "/**")
            then
               Last := Last - 3;
               Recursively := True;
            end if;

            if Project_File then

               --  Add the directory in the list for attribute Source_Dirs

               declare
                  Expression : constant Project_Node_Id :=
                    Default_Project_Node
                    (Of_Kind => N_Expression,
                     And_Expr_Kind => Single);

                  Term : constant Project_Node_Id :=
                    Default_Project_Node
                    (Of_Kind => N_Term,
                     And_Expr_Kind => Single);

                  Value : constant Project_Node_Id :=
                    Default_Project_Node
                    (Of_Kind => N_Literal_String,
                     And_Expr_Kind => Single);

               begin
                  if Current_Source_Dir = Empty_Node then
                     Set_First_Expression_In_List
                       (Source_Dirs_List, To => Expression);
                  else
                     Set_Next_Expression_In_List
                       (Current_Source_Dir, To => Expression);
                  end if;

                  Current_Source_Dir := Expression;
                  Set_First_Term (Expression, To => Term);
                  Set_Current_Term (Term, To => Value);
                  Name_Len := Dir_Name'Length;
                  Name_Buffer (1 .. Name_Len) := Dir_Name;
                  Set_String_Value_Of (Value, To => Name_Find);
               end;
            end if;

            Process_Directory (Dir_Name (Dir_Name'First .. Last), Recursively);
         end;

      end loop;

      if Project_File then
         Close (Source_List_FD);
      end if;

      declare
         Discard : Boolean;

      begin
         --  Delete the file if it already exists

         Delete_File
           (Path_Name (Directory_Last + 1 .. Path_Last),
            Success => Discard);

         --  Create a new one

         if Opt.Verbose_Mode then
            Output.Write_Str ("Creating new file """);
            Output.Write_Str (Path_Name (Directory_Last + 1 .. Path_Last));
            Output.Write_Line ("""");
         end if;

         Output_FD := Create_New_File
           (Path_Name (Directory_Last + 1 .. Path_Last),
            Fmode => Text);

         --  Fails if project file cannot be created

         if Output_FD = Invalid_FD then
            Prj.Com.Fail
              ("cannot create new """, Path_Name (1 .. Path_Last), """");
         end if;

         if Project_File then

            --  Output the project file

            Prj.PP.Pretty_Print
              (Project_Node,
               W_Char => Write_A_Char'Access,
               W_Eol  => Write_Eol'Access,
               W_Str  => Write_A_String'Access,
               Backward_Compatibility => False);
            Close (Output_FD);

            --  Delete the naming project file if it already exists

            Delete_File
              (Project_Naming_File_Name (1 .. Project_Naming_Last),
               Success => Discard);

            --  Create a new one

            if Opt.Verbose_Mode then
               Output.Write_Str ("Creating new naming project file """);
               Output.Write_Str (Project_Naming_File_Name
                                   (1 .. Project_Naming_Last));
               Output.Write_Line ("""");
            end if;

            Output_FD := Create_New_File
              (Project_Naming_File_Name (1 .. Project_Naming_Last),
               Fmode => Text);

            --  Fails if naming project file cannot be created

            if Output_FD = Invalid_FD then
               Prj.Com.Fail
                 ("cannot create new """,
                  Project_Naming_File_Name (1 .. Project_Naming_Last),
                  """");
            end if;

            --  Output the naming project file

            Prj.PP.Pretty_Print
              (Project_Naming_Node,
               W_Char => Write_A_Char'Access,
               W_Eol  => Write_Eol'Access,
               W_Str  => Write_A_String'Access,
               Backward_Compatibility => False);
            Close (Output_FD);

         else
            --  Write to the output file each entry in the SFN_Pragmas table
            --  as an pragma Source_File_Name.

            for Index in 1 .. SFN_Pragmas.Last loop
               Write_A_String ("pragma Source_File_Name");
               Write_Eol;
               Write_A_String ("  (");
               Write_A_String (SFN_Pragmas.Table (Index).Unit.all);
               Write_A_String (",");
               Write_Eol;

               if SFN_Pragmas.Table (Index).Spec then
                  Write_A_String ("   Spec_File_Name => """);

               else
                  Write_A_String ("   Body_File_Name => """);
               end if;

               Write_A_String (SFN_Pragmas.Table (Index).File.all);
               Write_A_String (""");");
               Write_Eol;
            end loop;

            Close (Output_FD);
         end if;
      end;

   end Make;

   ----------------
   -- Write_Char --
   ----------------
   procedure Write_A_Char (C : Character) is
   begin
      Write_A_String ((1 => C));
   end Write_A_Char;

   ---------------
   -- Write_Eol --
   ---------------

   procedure Write_Eol is
   begin
      Write_A_String ((1 => ASCII.LF));
   end Write_Eol;

   --------------------
   -- Write_A_String --
   --------------------

   procedure Write_A_String (S : String) is
      Str : String (1 .. S'Length);

   begin
      if S'Length > 0 then
         Str := S;

         if Write (Output_FD, Str (1)'Address, Str'Length) /= Str'Length then
            Prj.Com.Fail ("disk full");
         end if;
      end if;
   end Write_A_String;

end Prj.Makr;
