------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P R J . P A R T                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 2001-2002 Free Software Foundation, Inc.          --
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

with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Exceptions;             use Ada.Exceptions;
with Errout;                     use Errout;
with GNAT.Directory_Operations;  use GNAT.Directory_Operations;
with GNAT.OS_Lib;                use GNAT.OS_Lib;
with Namet;                      use Namet;
with Osint;                      use Osint;
with Output;                     use Output;
with Prj.Com;                    use Prj.Com;
with Prj.Dect;
with Scans;                      use Scans;
with Scn;                        use Scn;
with Sinfo;                      use Sinfo;
with Sinput;                     use Sinput;
with Sinput.P;                   use Sinput.P;
with Stringt;                    use Stringt;
with Table;
with Types;                      use Types;

pragma Elaborate_All (GNAT.OS_Lib);

package body Prj.Part is

   Dir_Sep  : Character renames GNAT.OS_Lib.Directory_Separator;

   Project_Path : String_Access;
   --  The project path; initialized during package elaboration.

   Ada_Project_Path : constant String := "ADA_PROJECT_PATH";
   Prj_Path : constant String_Access := Getenv (Ada_Project_Path);

   ------------------------------------
   -- Local Packages and Subprograms --
   ------------------------------------

   package Project_Stack is new Table.Table
     (Table_Component_Type => Name_Id,
      Table_Index_Type     => Nat,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 10,
      Table_Name           => "Prj.Part.Project_Stack");
   --  This table is used to detect circular dependencies
   --  for imported and modified projects.

   procedure Parse_Context_Clause
     (Context_Clause    : out Project_Node_Id;
      Project_Directory : Name_Id);
   --  Parse the context clause of a project
   --  Does nothing if there is b\no context clause (if the current
   --  token is not "with").

   procedure Parse_Single_Project
     (Project         : out Project_Node_Id;
      Path_Name       : String;
      Modified        : Boolean);
   --  Parse a project file.
   --  Recursive procedure: it calls itself for imported and
   --  modified projects.

   function Project_Path_Name_Of
     (Project_File_Name : String;
      Directory         : String)
      return              String;
   --  Returns the path name of a project file.
   --  Returns an empty string if project file cannot be found.

   function Immediate_Directory_Of (Path_Name : Name_Id) return Name_Id;
   --  Get the directory of the file with the specified path name.
   --  This includes the directory separator as the last character.
   --  Returns "./" if Path_Name contains no directory separator.

   function Simple_File_Name_Of (Path_Name : Name_Id) return Name_Id;
   --  Returns the name of a file with the specified path name
   --  with no directory information.

   function Project_Name_From (Path_Name : String) return Name_Id;
   --  Returns the name of the project that corresponds to its path name.
   --  Returns No_Name if the path name is invalid, because the corresponding
   --  project name does not have the syntax of an ada identifier.

   ----------------------------
   -- Immediate_Directory_Of --
   ----------------------------

   function Immediate_Directory_Of (Path_Name : Name_Id) return Name_Id is
   begin
      Get_Name_String (Path_Name);

      for Index in reverse 1 .. Name_Len loop
         if Name_Buffer (Index) = '/'
           or else Name_Buffer (Index) = Dir_Sep
         then
            --  Remove from name all characters after the last
            --  directory separator.

            Name_Len := Index;
            return Name_Find;
         end if;
      end loop;

      --  There is no directory separator in name. Return "./" or ".\"

      Name_Len := 2;
      Name_Buffer (1) := '.';
      Name_Buffer (2) := Dir_Sep;
      return Name_Find;
   end Immediate_Directory_Of;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Project                : out Project_Node_Id;
      Project_File_Name      : String;
      Always_Errout_Finalize : Boolean)
   is
      Current_Directory : constant String := Get_Current_Dir;

   begin
      Project := Empty_Node;

      if Current_Verbosity >= Medium then
         Write_Str ("ADA_PROJECT_PATH=""");
         Write_Str (Project_Path.all);
         Write_Line ("""");
      end if;

      declare
         Path_Name : constant String :=
                       Project_Path_Name_Of (Project_File_Name,
                                             Directory   => Current_Directory);

      begin
         Errout.Initialize;

         --  Parse the main project file

         if Path_Name = "" then
            Fail ("project file """ & Project_File_Name & """ not found");
         end if;

         Parse_Single_Project
           (Project         => Project,
            Path_Name       => Path_Name,
            Modified        => False);

         --  If there were any kind of error during the parsing, serious
         --  or not, then the parsing fails.

         if Errout.Total_Errors_Detected > 0 then
            Project := Empty_Node;
         end if;

         if Project = Empty_Node or else Always_Errout_Finalize then
            Errout.Finalize;
         end if;
      end;

   exception
      when X : others =>

         --  Internal error

         Write_Line (Exception_Information (X));
         Write_Str  ("Exception ");
         Write_Str  (Exception_Name (X));
         Write_Line (" raised, while processing project file");
         Project := Empty_Node;
   end Parse;

   --------------------------
   -- Parse_Context_Clause --
   --------------------------

   procedure Parse_Context_Clause
     (Context_Clause    : out Project_Node_Id;
      Project_Directory : Name_Id)
   is
      Project_Directory_Path : constant String :=
                                 Get_Name_String (Project_Directory);
      Current_With_Clause    : Project_Node_Id := Empty_Node;
      Next_With_Clause       : Project_Node_Id := Empty_Node;

   begin
      --  Assume no context clause

      Context_Clause := Empty_Node;
      With_Loop :

      --  If Token is not WITH, there is no context clause,
      --  or we have exhausted the with clauses.

      while Token = Tok_With loop
         Comma_Loop :
         loop
            Scan; -- scan past WITH or ","

            Expect (Tok_String_Literal, "literal string");

            if Token /= Tok_String_Literal then
               return;
            end if;

            String_To_Name_Buffer (Strval (Token_Node));

            declare
               Original_Path : constant String :=
                                 Name_Buffer (1 .. Name_Len);

               Imported_Path_Name : constant String :=
                                      Project_Path_Name_Of
                                        (Original_Path,
                                         Project_Directory_Path);

               Withed_Project : Project_Node_Id := Empty_Node;

            begin
               if Imported_Path_Name = "" then

                  --  The project file cannot be found

                  Name_Len := Original_Path'Length;
                  Name_Buffer (1 .. Name_Len) := Original_Path;
                  Error_Msg_Name_1 := Name_Find;

                  Error_Msg ("unknown project file: {", Token_Ptr);

                  --  If this is not imported by the main project file,
                  --  display the import path.

                  if Project_Stack.Last > 1 then
                     for Index in reverse 1 .. Project_Stack.Last loop
                        Error_Msg_Name_1 := Project_Stack.Table (Index);
                        Error_Msg ("\imported by {", Token_Ptr);
                     end loop;
                  end if;

               else
                  --  New with clause

                  if Current_With_Clause = Empty_Node then

                     --  First with clause of the context clause

                     Current_With_Clause := Default_Project_Node
                       (Of_Kind => N_With_Clause);
                     Context_Clause := Current_With_Clause;

                  else
                     Next_With_Clause := Default_Project_Node
                       (Of_Kind => N_With_Clause);
                     Set_Next_With_Clause_Of
                       (Current_With_Clause, Next_With_Clause);
                     Current_With_Clause := Next_With_Clause;
                  end if;

                  Set_String_Value_Of
                    (Current_With_Clause, Strval (Token_Node));
                  Set_Location_Of (Current_With_Clause, Token_Ptr);
                  String_To_Name_Buffer
                    (String_Value_Of (Current_With_Clause));

                  --  Parse the imported project

                  Parse_Single_Project
                    (Project   => Withed_Project,
                     Path_Name => Imported_Path_Name,
                     Modified  => False);

                  if Withed_Project /= Empty_Node then

                     --  If parsing was successful, record project name
                     --  and path name in with clause

                     Set_Project_Node_Of (Current_With_Clause, Withed_Project);
                     Set_Name_Of (Current_With_Clause,
                                  Name_Of (Withed_Project));
                     Name_Len := Imported_Path_Name'Length;
                     Name_Buffer (1 .. Name_Len) := Imported_Path_Name;
                     Set_Path_Name_Of (Current_With_Clause, Name_Find);
                  end if;
               end if;
            end;

            Scan;
            if Token = Tok_Semicolon then

               --  End of (possibly multiple) with clause;

               Scan; -- scan past the semicolon.
               exit Comma_Loop;

            elsif Token /= Tok_Comma then
               Error_Msg ("expected comma or semi colon", Token_Ptr);
               exit Comma_Loop;
            end if;
         end loop Comma_Loop;
      end loop With_Loop;

   end Parse_Context_Clause;

   --------------------------
   -- Parse_Single_Project --
   --------------------------

   procedure Parse_Single_Project
     (Project         : out Project_Node_Id;
      Path_Name       : String;
      Modified        : Boolean)
   is
      Canonical_Path_Name : Name_Id;
      Project_Directory   : Name_Id;
      Project_Scan_State  : Saved_Project_Scan_State;
      Source_Index        : Source_File_Index;

      Modified_Project    : Project_Node_Id := Empty_Node;

      A_Project_Name_And_Node : Tree_Private_Part.Project_Name_And_Node :=
        Tree_Private_Part.Projects_Htable.Get_First;

      Name_From_Path : constant Name_Id := Project_Name_From (Path_Name);

      use Tree_Private_Part;

   begin
      Name_Len := Path_Name'Length;
      Name_Buffer (1 .. Name_Len) := Path_Name;
      Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
      Canonical_Path_Name := Name_Find;

      --  Check for a circular dependency

      for Index in 1 .. Project_Stack.Last loop
         if Canonical_Path_Name = Project_Stack.Table (Index) then
            Error_Msg ("circular dependency detected", Token_Ptr);
            Error_Msg_Name_1 := Canonical_Path_Name;
            Error_Msg ("\  { is imported by", Token_Ptr);

            for Current in reverse 1 .. Project_Stack.Last loop
               Error_Msg_Name_1 := Project_Stack.Table (Current);

               if Error_Msg_Name_1 /= Canonical_Path_Name then
                  Error_Msg
                    ("\  { which itself is imported by", Token_Ptr);

               else
                  Error_Msg ("\  {", Token_Ptr);
                  exit;
               end if;
            end loop;

            Project := Empty_Node;
            return;
         end if;
      end loop;

      Project_Stack.Increment_Last;
      Project_Stack.Table (Project_Stack.Last) := Canonical_Path_Name;

      --  Check if the project file has already been parsed.

      while
        A_Project_Name_And_Node /= Tree_Private_Part.No_Project_Name_And_Node
      loop
         if
           Path_Name_Of (A_Project_Name_And_Node.Node) = Canonical_Path_Name
         then
            if Modified then

               if A_Project_Name_And_Node.Modified then
                  Error_Msg
                    ("cannot modify the same project file several times",
                     Token_Ptr);

               else
                  Error_Msg
                    ("cannot modify an imported project file",
                     Token_Ptr);
               end if;

            elsif A_Project_Name_And_Node.Modified then
               Error_Msg
                 ("cannot imported a modified project file",
                  Token_Ptr);
            end if;

            Project := A_Project_Name_And_Node.Node;
            Project_Stack.Decrement_Last;
            return;
         end if;

         A_Project_Name_And_Node := Tree_Private_Part.Projects_Htable.Get_Next;
      end loop;

      --  We never encountered this project file
      --  Save the scan state, load the project file and start to scan it.

      Save_Project_Scan_State (Project_Scan_State);
      Source_Index := Load_Project_File (Path_Name);

      --  if we cannot find it, we stop

      if Source_Index = No_Source_File then
         Project := Empty_Node;
         Project_Stack.Decrement_Last;
         return;
      end if;

      Initialize_Scanner (Types.No_Unit, Source_Index);

      if Name_From_Path = No_Name then

         --  The project file name is not correct (no or bad extension,
         --  or not following Ada identifier's syntax).

         Error_Msg_Name_1 := Canonical_Path_Name;
         Error_Msg ("?{ is not a valid path name for a project file",
                    Token_Ptr);
      end if;

      if Current_Verbosity >= Medium then
         Write_Str  ("Parsing """);
         Write_Str  (Path_Name);
         Write_Char ('"');
         Write_Eol;
      end if;

      Project_Directory := Immediate_Directory_Of (Canonical_Path_Name);
      Project := Default_Project_Node (Of_Kind => N_Project);
      Set_Directory_Of (Project, Project_Directory);
      Set_Name_Of (Project, Simple_File_Name_Of (Canonical_Path_Name));
      Set_Path_Name_Of (Project, Canonical_Path_Name);
      Set_Location_Of (Project, Token_Ptr);

      --  Is there any imported project?

      declare
         First_With_Clause : Project_Node_Id := Empty_Node;

      begin
         Parse_Context_Clause (Context_Clause    => First_With_Clause,
                               Project_Directory => Project_Directory);
         Set_First_With_Clause_Of (Project, First_With_Clause);
      end;

      Expect (Tok_Project, "project");

      --  Mark location of PROJECT token if present

      if Token = Tok_Project then
         Set_Location_Of (Project, Token_Ptr);
         Scan; -- scan past project
      end if;

      Expect (Tok_Identifier, "identifier");

      if Token = Tok_Identifier then
         Set_Name_Of (Project, Token_Name);

         Get_Name_String (Token_Name);
         Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));

         declare
            Expected_Name : constant Name_Id := Name_Find;

         begin
            if Name_From_Path /= No_Name
              and then Expected_Name /= Name_From_Path
            then
               --  The project name is not the one that was expected from
               --  the file name. Report a warning.

               Error_Msg_Name_1 := Expected_Name;
               Error_Msg ("?file name does not match unit name, " &
                          "should be `{" & Project_File_Extension & "`",
                          Token_Ptr);
            end if;
         end;

         declare
            Project_Name : Name_Id :=
                             Tree_Private_Part.Projects_Htable.Get_First.Name;

         begin
            --  Check if we already have a project with this name

            while Project_Name /= No_Name
              and then Project_Name /= Token_Name
            loop
               Project_Name := Tree_Private_Part.Projects_Htable.Get_Next.Name;
            end loop;

            if Project_Name /= No_Name then
               Error_Msg ("duplicate project name", Token_Ptr);

            else
               Tree_Private_Part.Projects_Htable.Set
                 (K => Token_Name,
                  E => (Name     => Token_Name,
                        Node     => Project,
                        Modified => Modified));
            end if;
         end;

         Scan; -- scan past the project name
      end if;

      if Token = Tok_Extends then

         --  We are extending another project

         Scan; -- scan past EXTENDS
         Expect (Tok_String_Literal, "literal string");

         if Token = Tok_String_Literal then
            Set_Modified_Project_Path_Of (Project, Strval (Token_Node));
            String_To_Name_Buffer (Modified_Project_Path_Of (Project));

            declare
               Original_Path_Name : constant String :=
                                      Name_Buffer (1 .. Name_Len);

               Modified_Project_Path_Name : constant String :=
                                              Project_Path_Name_Of
                                                (Original_Path_Name,
                                                   Get_Name_String
                                                     (Project_Directory));

            begin
               if Modified_Project_Path_Name = "" then

                  --  We could not find the project file to modify

                  Name_Len := Original_Path_Name'Length;
                  Name_Buffer (1 .. Name_Len) := Original_Path_Name;
                  Error_Msg_Name_1 := Name_Find;

                  Error_Msg ("unknown project file: {", Token_Ptr);

                  --  If we are not in the main project file, display the
                  --  import path.

                  if Project_Stack.Last > 1 then
                     Error_Msg_Name_1 :=
                       Project_Stack.Table (Project_Stack.Last);
                     Error_Msg ("\extended by {", Token_Ptr);

                     for Index in reverse 1 .. Project_Stack.Last - 1 loop
                        Error_Msg_Name_1 := Project_Stack.Table (Index);
                        Error_Msg ("\imported by {", Token_Ptr);
                     end loop;
                  end if;

               else
                  Parse_Single_Project
                    (Project   => Modified_Project,
                     Path_Name => Modified_Project_Path_Name,
                     Modified  => True);
               end if;
            end;

            Scan; -- scan past the modified project path
         end if;
      end if;

      Expect (Tok_Is, "is");

      declare
         Project_Declaration : Project_Node_Id := Empty_Node;

      begin
         --  No need to Scan past IS, Prj.Dect.Parse will do it.

         Prj.Dect.Parse
           (Declarations    => Project_Declaration,
            Current_Project => Project,
            Extends         => Modified_Project);
         Set_Project_Declaration_Of (Project, Project_Declaration);
      end;

      Expect (Tok_End, "end");

      --  Skip END if present

      if Token = Tok_End then
         Scan;
      end if;

      Expect (Tok_Identifier, "identifier");

      if Token = Tok_Identifier then

         --  We check if this is the project name

         if To_Lower (Get_Name_String (Token_Name)) /=
            Get_Name_String (Name_Of (Project))
         then
            Error_Msg ("Expected """ &
                       Get_Name_String (Name_Of (Project)) & """",
                       Token_Ptr);
         end if;
      end if;

      if Token /= Tok_Semicolon then
         Scan;
      end if;

      Expect (Tok_Semicolon, ";");

      --  Restore the scan state, in case we are not the main project

      Restore_Project_Scan_State (Project_Scan_State);

      Project_Stack.Decrement_Last;
   end Parse_Single_Project;

   -----------------------
   -- Project_Name_From --
   -----------------------

   function Project_Name_From (Path_Name : String) return Name_Id is
      Canonical : String (1 .. Path_Name'Length) := Path_Name;
      First : Natural  := Canonical'Last;
      Last  : Positive := First;

   begin
      if First = 0 then
         return No_Name;
      end if;

      Canonical_Case_File_Name (Canonical);

      while First > 0
        and then
        Canonical (First) /= '.'
      loop
         First := First - 1;
      end loop;

      if Canonical (First) = '.' then
         if Canonical (First .. Last) = Project_File_Extension
           and then First /= 1
         then
            First := First - 1;
            Last := First;

            while First > 0
              and then Canonical (First) /= '/'
              and then Canonical (First) /= Dir_Sep
            loop
               First := First - 1;
            end loop;

         else
            return No_Name;
         end if;

      else
         return No_Name;
      end if;

      if Canonical (First) = '/'
        or else Canonical (First) = Dir_Sep
      then
         First := First + 1;
      end if;

      Name_Len := Last - First + 1;
      Name_Buffer (1 .. Name_Len) := To_Lower (Canonical (First .. Last));

      if not Is_Letter (Name_Buffer (1)) then
         return No_Name;

      else
         for Index in 2 .. Name_Len - 1 loop
            if Name_Buffer (Index) = '_' then
               if Name_Buffer (Index + 1) = '_' then
                  return No_Name;
               end if;

            elsif not Is_Alphanumeric (Name_Buffer (Index)) then
               return No_Name;
            end if;

         end loop;

         if not Is_Alphanumeric (Name_Buffer (Name_Len)) then
            return No_Name;

         else
            return Name_Find;
         end if;

      end if;
   end Project_Name_From;

   --------------------------
   -- Project_Path_Name_Of --
   --------------------------

   function Project_Path_Name_Of
     (Project_File_Name : String;
      Directory         : String)
      return              String
   is
      Result : String_Access;

   begin
      --  First we try <file_name>.<extension>

      if Current_Verbosity = High then
         Write_Str  ("Project_Path_Name_Of (""");
         Write_Str  (Project_File_Name);
         Write_Str  (""", """);
         Write_Str  (Directory);
         Write_Line (""");");
         Write_Str  ("   Trying ");
         Write_Str (Project_File_Name);
         Write_Line (Project_File_Extension);
      end if;

      Result :=
        Locate_Regular_File
          (File_Name => Project_File_Name & Project_File_Extension,
           Path      => Project_Path.all);

      --  Then we try <file_name>

      if Result = null then
         if Current_Verbosity = High then
            Write_Str  ("   Trying ");
            Write_Line  (Project_File_Name);
         end if;

         Result :=
           Locate_Regular_File
           (File_Name => Project_File_Name,
            Path      => Project_Path.all);

         --  The we try <directory>/<file_name>.<extension>

         if Result = null then
            if Current_Verbosity = High then
               Write_Str  ("   Trying ");
               Write_Str  (Directory);
               Write_Str (Project_File_Name);
               Write_Line (Project_File_Extension);
            end if;

            Result :=
              Locate_Regular_File
              (File_Name => Directory & Project_File_Name &
                            Project_File_Extension,
               Path      => Project_Path.all);

            --  Then we try <directory>/<file_name>

            if Result = null then
               if Current_Verbosity = High then
                  Write_Str  ("   Trying ");
                  Write_Str  (Directory);
                  Write_Line  (Project_File_Name);
               end if;

               Result :=
                 Locate_Regular_File
                 (File_Name => Directory & Project_File_Name,
                  Path      => Project_Path.all);
            end if;
         end if;
      end if;

      --  If we cannot find the project file, we return an empty string

      if Result = null then
         return "";

      else
         declare
            Final_Result : String
              := GNAT.OS_Lib.Normalize_Pathname (Result.all);
         begin
            Free (Result);
            Canonical_Case_File_Name (Final_Result);
            return Final_Result;
         end;

      end if;

   end Project_Path_Name_Of;

   -------------------------
   -- Simple_File_Name_Of --
   -------------------------

   function Simple_File_Name_Of (Path_Name : Name_Id) return Name_Id is
   begin
      Get_Name_String (Path_Name);

      for Index in reverse 1 .. Name_Len loop
         if Name_Buffer (Index) = '/'
           or else Name_Buffer (Index) = Dir_Sep
         then
            exit when Index = Name_Len;
            Name_Buffer (1 .. Name_Len - Index) :=
              Name_Buffer (Index + 1 .. Name_Len);
            Name_Len := Name_Len - Index;
            return Name_Find;
         end if;
      end loop;

      return No_Name;

   end Simple_File_Name_Of;

begin
   if Prj_Path.all = "" then
      Project_Path := new String'(".");

   else
      Project_Path := new String'("." & Path_Separator & Prj_Path.all);
   end if;

end Prj.Part;
