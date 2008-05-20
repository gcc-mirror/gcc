------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P R J . S T R T                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2001-2008, Free Software Foundation, Inc.         --
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

with Err_Vars; use Err_Vars;
with Prj.Attr; use Prj.Attr;
with Prj.Err;  use Prj.Err;
with Snames;
with Table;
with Uintp;    use Uintp;

package body Prj.Strt is

   Buffer      : String_Access;
   Buffer_Last : Natural := 0;

   type Choice_String is record
      The_String   : Name_Id;
      Already_Used : Boolean := False;
   end record;
   --  The string of a case label, and an indication that it has already
   --  been used (to avoid duplicate case labels).

   Choices_Initial   : constant := 10;
   Choices_Increment : constant := 100;
   --  These should be in alloc.ads

   Choice_Node_Low_Bound  : constant := 0;
   Choice_Node_High_Bound : constant := 099_999_999;
   --  In practice, infinite

   type Choice_Node_Id is
     range Choice_Node_Low_Bound .. Choice_Node_High_Bound;

   First_Choice_Node_Id : constant Choice_Node_Id :=
     Choice_Node_Low_Bound;

   package Choices is
     new Table.Table
       (Table_Component_Type => Choice_String,
        Table_Index_Type     => Choice_Node_Id'Base,
        Table_Low_Bound      => First_Choice_Node_Id,
        Table_Initial        => Choices_Initial,
        Table_Increment      => Choices_Increment,
        Table_Name           => "Prj.Strt.Choices");
   --  Used to store the case labels and check that there is no duplicate

   package Choice_Lasts is
     new Table.Table
       (Table_Component_Type => Choice_Node_Id,
        Table_Index_Type     => Nat,
        Table_Low_Bound      => 1,
        Table_Initial        => 10,
        Table_Increment      => 100,
        Table_Name           => "Prj.Strt.Choice_Lasts");
   --  Used to store the indices of the choices in table Choices,
   --  to distinguish nested case constructions.

   Choice_First : Choice_Node_Id := 0;
   --  Index in table Choices of the first case label of the current
   --  case construction. Zero means no current case construction.

   type Name_Location is record
      Name     : Name_Id := No_Name;
      Location : Source_Ptr := No_Location;
   end record;
   --  Store the identifier and the location of a simple name

   package Names is
     new Table.Table
       (Table_Component_Type => Name_Location,
        Table_Index_Type     => Nat,
        Table_Low_Bound      => 1,
        Table_Initial        => 10,
        Table_Increment      => 100,
        Table_Name           => "Prj.Strt.Names");
   --  Used to accumulate the single names of a name

   procedure Add (This_String : Name_Id);
   --  Add a string to the case label list, indicating that it has not
   --  yet been used.

   procedure Add_To_Names (NL : Name_Location);
   --  Add one single names to table Names

   procedure External_Reference
     (In_Tree         : Project_Node_Tree_Ref;
      Current_Project : Project_Node_Id;
      Current_Package : Project_Node_Id;
      External_Value  : out Project_Node_Id);
   --  Parse an external reference. Current token is "external"

   procedure Attribute_Reference
     (In_Tree         : Project_Node_Tree_Ref;
      Reference       : out Project_Node_Id;
      First_Attribute : Attribute_Node_Id;
      Current_Project : Project_Node_Id;
      Current_Package : Project_Node_Id);
   --  Parse an attribute reference. Current token is an apostrophe

   procedure Terms
     (In_Tree         : Project_Node_Tree_Ref;
      Term            : out Project_Node_Id;
      Expr_Kind       : in out Variable_Kind;
      Current_Project : Project_Node_Id;
      Current_Package : Project_Node_Id;
      Optional_Index  : Boolean);
   --  Recursive procedure to parse one term or several terms concatenated
   --  using "&".

   ---------
   -- Add --
   ---------

   procedure Add (This_String : Name_Id) is
   begin
      Choices.Increment_Last;
      Choices.Table (Choices.Last) :=
        (The_String   => This_String,
         Already_Used => False);
   end Add;

   ------------------
   -- Add_To_Names --
   ------------------

   procedure Add_To_Names (NL : Name_Location) is
   begin
      Names.Increment_Last;
      Names.Table (Names.Last) := NL;
   end Add_To_Names;

   -------------------------
   -- Attribute_Reference --
   -------------------------

   procedure Attribute_Reference
     (In_Tree         : Project_Node_Tree_Ref;
      Reference       : out Project_Node_Id;
      First_Attribute : Attribute_Node_Id;
      Current_Project : Project_Node_Id;
      Current_Package : Project_Node_Id)
   is
      Current_Attribute : Attribute_Node_Id := First_Attribute;

   begin
      --  Declare the node of the attribute reference

      Reference :=
        Default_Project_Node
          (Of_Kind => N_Attribute_Reference, In_Tree => In_Tree);
      Set_Location_Of (Reference, In_Tree, To => Token_Ptr);
      Scan (In_Tree); --  past apostrophe

      --  Body may be an attribute name

      if Token = Tok_Body then
         Token      := Tok_Identifier;
         Token_Name := Snames.Name_Body;
      end if;

      Expect (Tok_Identifier, "identifier");

      if Token = Tok_Identifier then
         Set_Name_Of (Reference, In_Tree, To => Token_Name);

         --  Check if the identifier is one of the attribute identifiers in the
         --  context (package or project level attributes).

         Current_Attribute :=
           Attribute_Node_Id_Of (Token_Name, Starting_At => First_Attribute);

         --  If the identifier is not allowed, report an error

         if Current_Attribute = Empty_Attribute then
            Error_Msg_Name_1 := Token_Name;
            Error_Msg ("unknown attribute %%", Token_Ptr);
            Reference := Empty_Node;

            --  Scan past the attribute name

            Scan (In_Tree);

         else
            --  Give its characteristics to this attribute reference

            Set_Project_Node_Of (Reference, In_Tree, To => Current_Project);
            Set_Package_Node_Of (Reference, In_Tree, To => Current_Package);
            Set_Expression_Kind_Of
              (Reference, In_Tree, To => Variable_Kind_Of (Current_Attribute));
            Set_Case_Insensitive
              (Reference, In_Tree,
               To => Attribute_Kind_Of (Current_Attribute) in
                      Case_Insensitive_Associative_Array ..
                        Optional_Index_Case_Insensitive_Associative_Array);

            --  Scan past the attribute name

            Scan (In_Tree);

            --  If the attribute is an associative array, get the index

            if Attribute_Kind_Of (Current_Attribute) /= Single then
               Expect (Tok_Left_Paren, "`(`");

               if Token = Tok_Left_Paren then
                  Scan (In_Tree);
                  Expect (Tok_String_Literal, "literal string");

                  if Token = Tok_String_Literal then
                     Set_Associative_Array_Index_Of
                       (Reference, In_Tree, To => Token_Name);
                     Scan (In_Tree);
                     Expect (Tok_Right_Paren, "`)`");

                     if Token = Tok_Right_Paren then
                        Scan (In_Tree);
                     end if;
                  end if;
               end if;
            end if;
         end if;

         --  Change name of obsolete attributes

         if Present (Reference) then
            case Name_Of (Reference, In_Tree) is
               when Snames.Name_Specification =>
                  Set_Name_Of (Reference, In_Tree, To => Snames.Name_Spec);

               when Snames.Name_Specification_Suffix =>
                  Set_Name_Of
                    (Reference, In_Tree, To => Snames.Name_Spec_Suffix);

               when Snames.Name_Implementation =>
                  Set_Name_Of (Reference, In_Tree, To => Snames.Name_Body);

               when Snames.Name_Implementation_Suffix =>
                  Set_Name_Of
                    (Reference, In_Tree, To => Snames.Name_Body_Suffix);

               when others =>
                  null;
            end case;
         end if;
      end if;
   end Attribute_Reference;

   ---------------------------
   -- End_Case_Construction --
   ---------------------------

   procedure End_Case_Construction
     (Check_All_Labels   : Boolean;
      Case_Location      : Source_Ptr)
   is
      Non_Used : Natural := 0;
      First_Non_Used : Choice_Node_Id := First_Choice_Node_Id;
   begin
      --  First, if Check_All_Labels is True, check if all values
      --  of the string type have been used.

      if Check_All_Labels then
         for Choice in Choice_First .. Choices.Last loop
               if not Choices.Table (Choice).Already_Used then
                  Non_Used := Non_Used + 1;

                  if Non_Used = 1 then
                     First_Non_Used := Choice;
                  end if;
               end if;
         end loop;

         --  If only one is not used, report a single warning for this value

         if Non_Used = 1 then
            Error_Msg_Name_1 := Choices.Table (First_Non_Used).The_String;
            Error_Msg ("?value %% is not used as label", Case_Location);

         --  If several are not used, report a warning for each one of them

         elsif Non_Used > 1 then
            Error_Msg
              ("?the following values are not used as labels:",
               Case_Location);

            for Choice in First_Non_Used .. Choices.Last loop
               if not Choices.Table (Choice).Already_Used then
                  Error_Msg_Name_1 := Choices.Table (Choice).The_String;
                  Error_Msg ("\?%%", Case_Location);
               end if;
            end loop;
         end if;
      end if;

      --  If this is the only case construction, empty the tables

      if Choice_Lasts.Last = 1 then
         Choice_Lasts.Set_Last (0);
         Choices.Set_Last (First_Choice_Node_Id);
         Choice_First := 0;

      elsif Choice_Lasts.Last = 2 then

         --  This is the second case construction, set the tables to the first

         Choice_Lasts.Set_Last (1);
         Choices.Set_Last (Choice_Lasts.Table (1));
         Choice_First := 1;

      else
         --  This is the 3rd or more case construction, set the tables to the
         --  previous one.

         Choice_Lasts.Decrement_Last;
         Choices.Set_Last (Choice_Lasts.Table (Choice_Lasts.Last));
         Choice_First := Choice_Lasts.Table (Choice_Lasts.Last - 1) + 1;
      end if;
   end End_Case_Construction;

   ------------------------
   -- External_Reference --
   ------------------------

   procedure External_Reference
     (In_Tree         : Project_Node_Tree_Ref;
      Current_Project : Project_Node_Id;
      Current_Package : Project_Node_Id;
      External_Value  : out Project_Node_Id)
   is
      Field_Id : Project_Node_Id := Empty_Node;

   begin
      External_Value :=
        Default_Project_Node
          (Of_Kind       => N_External_Value,
           In_Tree       => In_Tree,
           And_Expr_Kind => Single);
      Set_Location_Of (External_Value, In_Tree, To => Token_Ptr);

      --  The current token is External

      --  Get the left parenthesis

      Scan (In_Tree);
      Expect (Tok_Left_Paren, "`(`");

      --  Scan past the left parenthesis

      if Token = Tok_Left_Paren then
         Scan (In_Tree);
      end if;

      --  Get the name of the external reference

      Expect (Tok_String_Literal, "literal string");

      if Token = Tok_String_Literal then
         Field_Id :=
           Default_Project_Node
             (Of_Kind       => N_Literal_String,
              In_Tree       => In_Tree,
              And_Expr_Kind => Single);
         Set_String_Value_Of (Field_Id, In_Tree, To => Token_Name);
         Set_External_Reference_Of (External_Value, In_Tree, To => Field_Id);

         --  Scan past the first argument

         Scan (In_Tree);

         case Token is

            when Tok_Right_Paren =>
               Scan (In_Tree); -- scan past right paren

            when Tok_Comma =>
               Scan (In_Tree); -- scan past comma

               --  Get the string expression for the default

               declare
                  Loc : constant Source_Ptr := Token_Ptr;

               begin
                  Parse_Expression
                    (In_Tree         => In_Tree,
                     Expression      => Field_Id,
                     Current_Project => Current_Project,
                     Current_Package => Current_Package,
                     Optional_Index  => False);

                  if Expression_Kind_Of (Field_Id, In_Tree) = List then
                     Error_Msg ("expression must be a single string", Loc);
                  else
                     Set_External_Default_Of
                       (External_Value, In_Tree, To => Field_Id);
                  end if;
               end;

               Expect (Tok_Right_Paren, "`)`");

               if Token = Tok_Right_Paren then
                  Scan (In_Tree); -- scan past right paren
               end if;

            when others =>
               Error_Msg ("`,` or `)` expected", Token_Ptr);
         end case;
      end if;
   end External_Reference;

   -----------------------
   -- Parse_Choice_List --
   -----------------------

   procedure Parse_Choice_List
     (In_Tree      : Project_Node_Tree_Ref;
      First_Choice : out Project_Node_Id)
   is
      Current_Choice : Project_Node_Id := Empty_Node;
      Next_Choice    : Project_Node_Id := Empty_Node;
      Choice_String  : Name_Id         := No_Name;
      Found          : Boolean         := False;

   begin
      --  Declare the node of the first choice

      First_Choice :=
        Default_Project_Node
          (Of_Kind       => N_Literal_String,
           In_Tree       => In_Tree,
           And_Expr_Kind => Single);

      --  Initially Current_Choice is the same as First_Choice

      Current_Choice := First_Choice;

      loop
         Expect (Tok_String_Literal, "literal string");
         exit when Token /= Tok_String_Literal;
         Set_Location_Of (Current_Choice, In_Tree, To => Token_Ptr);
         Choice_String := Token_Name;

         --  Give the string value to the current choice

         Set_String_Value_Of (Current_Choice, In_Tree, To => Choice_String);

         --  Check if the label is part of the string type and if it has not
         --  been already used.

         Found := False;
         for Choice in Choice_First .. Choices.Last loop
            if Choices.Table (Choice).The_String = Choice_String then

               --  This label is part of the string type

               Found := True;

               if Choices.Table (Choice).Already_Used then

                  --  But it has already appeared in a choice list for this
                  --  case construction so report an error.

                  Error_Msg_Name_1 := Choice_String;
                  Error_Msg ("duplicate case label %%", Token_Ptr);

               else
                  Choices.Table (Choice).Already_Used := True;
               end if;

               exit;
            end if;
         end loop;

         --  If the label is not part of the string list, report an error

         if not Found then
            Error_Msg_Name_1 := Choice_String;
            Error_Msg ("illegal case label %%", Token_Ptr);
         end if;

         --  Scan past the label

         Scan (In_Tree);

         --  If there is no '|', we are done

         if Token = Tok_Vertical_Bar then

            --  Otherwise, declare the node of the next choice, link it to
            --  Current_Choice and set Current_Choice to this new node.

            Next_Choice :=
              Default_Project_Node
                (Of_Kind       => N_Literal_String,
                 In_Tree       => In_Tree,
                 And_Expr_Kind => Single);
            Set_Next_Literal_String
              (Current_Choice, In_Tree, To => Next_Choice);
            Current_Choice := Next_Choice;
            Scan (In_Tree);
         else
            exit;
         end if;
      end loop;
   end Parse_Choice_List;

   ----------------------
   -- Parse_Expression --
   ----------------------

   procedure Parse_Expression
     (In_Tree         : Project_Node_Tree_Ref;
      Expression      : out Project_Node_Id;
      Current_Project : Project_Node_Id;
      Current_Package : Project_Node_Id;
      Optional_Index  : Boolean)
   is
      First_Term      : Project_Node_Id := Empty_Node;
      Expression_Kind : Variable_Kind := Undefined;

   begin
      --  Declare the node of the expression

      Expression :=
        Default_Project_Node (Of_Kind => N_Expression, In_Tree => In_Tree);
      Set_Location_Of (Expression, In_Tree, To => Token_Ptr);

      --  Parse the term or terms of the expression

      Terms (In_Tree         => In_Tree,
             Term            => First_Term,
             Expr_Kind       => Expression_Kind,
             Current_Project => Current_Project,
             Current_Package => Current_Package,
             Optional_Index  => Optional_Index);

      --  Set the first term and the expression kind

      Set_First_Term (Expression, In_Tree, To => First_Term);
      Set_Expression_Kind_Of (Expression, In_Tree, To => Expression_Kind);
   end Parse_Expression;

   ----------------------------
   -- Parse_String_Type_List --
   ----------------------------

   procedure Parse_String_Type_List
     (In_Tree      : Project_Node_Tree_Ref;
      First_String : out Project_Node_Id)
   is
      Last_String  : Project_Node_Id := Empty_Node;
      Next_String  : Project_Node_Id := Empty_Node;
      String_Value : Name_Id         := No_Name;

   begin
      --  Declare the node of the first string

      First_String :=
        Default_Project_Node
          (Of_Kind       => N_Literal_String,
           In_Tree       => In_Tree,
           And_Expr_Kind => Single);

      --  Initially, Last_String is the same as First_String

      Last_String := First_String;

      loop
         Expect (Tok_String_Literal, "literal string");
         exit when Token /= Tok_String_Literal;
         String_Value := Token_Name;

         --  Give its string value to Last_String

         Set_String_Value_Of (Last_String, In_Tree, To => String_Value);
         Set_Location_Of (Last_String, In_Tree, To => Token_Ptr);

         --  Now, check if the string is already part of the string type

         declare
            Current : Project_Node_Id := First_String;

         begin
            while Current /= Last_String loop
               if String_Value_Of (Current, In_Tree) = String_Value then

                  --  This is a repetition, report an error

                  Error_Msg_Name_1 := String_Value;
                  Error_Msg ("duplicate value %% in type", Token_Ptr);
                  exit;
               end if;

               Current := Next_Literal_String (Current, In_Tree);
            end loop;
         end;

         --  Scan past the literal string

         Scan (In_Tree);

         --  If there is no comma following the literal string, we are done

         if Token /= Tok_Comma then
            exit;

         else
            --  Declare the next string, link it to Last_String and set
            --  Last_String to its node.

            Next_String :=
              Default_Project_Node
                (Of_Kind       => N_Literal_String,
                 In_Tree       => In_Tree,
                 And_Expr_Kind => Single);
            Set_Next_Literal_String (Last_String, In_Tree, To => Next_String);
            Last_String := Next_String;
            Scan (In_Tree);
         end if;
      end loop;
   end Parse_String_Type_List;

   ------------------------------
   -- Parse_Variable_Reference --
   ------------------------------

   procedure Parse_Variable_Reference
     (In_Tree         : Project_Node_Tree_Ref;
      Variable        : out Project_Node_Id;
      Current_Project : Project_Node_Id;
      Current_Package : Project_Node_Id)
   is
      Current_Variable : Project_Node_Id := Empty_Node;

      The_Package : Project_Node_Id := Current_Package;
      The_Project : Project_Node_Id := Current_Project;

      Specified_Project : Project_Node_Id   := Empty_Node;
      Specified_Package : Project_Node_Id   := Empty_Node;
      Look_For_Variable : Boolean           := True;
      First_Attribute   : Attribute_Node_Id := Empty_Attribute;
      Variable_Name     : Name_Id;

   begin
      Names.Init;

      loop
         Expect (Tok_Identifier, "identifier");

         if Token /= Tok_Identifier then
            Look_For_Variable := False;
            exit;
         end if;

         Add_To_Names (NL => (Name => Token_Name, Location => Token_Ptr));
         Scan (In_Tree);
         exit when Token /= Tok_Dot;
         Scan (In_Tree);
      end loop;

      if Look_For_Variable then

         if Token = Tok_Apostrophe then

            --  Attribute reference

            case Names.Last is
               when 0 =>

                  --  Cannot happen

                  null;

               when 1 =>
                  --  This may be a project name or a package name.
                  --  Project name have precedence.

                  --  First, look if it can be a package name

                  First_Attribute :=
                    First_Attribute_Of
                      (Package_Node_Id_Of (Names.Table (1).Name));

                  --  Now, look if it can be a project name

                  if Names.Table (1).Name =
                       Name_Of (Current_Project, In_Tree)
                  then
                     The_Project := Current_Project;

                  else
                     The_Project :=
                       Imported_Or_Extended_Project_Of
                         (Current_Project, In_Tree, Names.Table (1).Name);
                  end if;

                  if No (The_Project) then

                     --  If it is neither a project name nor a package name,
                     --  report an error.

                     if First_Attribute = Empty_Attribute then
                        Error_Msg_Name_1 := Names.Table (1).Name;
                        Error_Msg ("unknown project %",
                                   Names.Table (1).Location);
                        First_Attribute := Attribute_First;

                     else
                        --  If it is a package name, check if the package has
                        --  already been declared in the current project.

                        The_Package :=
                          First_Package_Of (Current_Project, In_Tree);

                        while Present (The_Package)
                          and then Name_Of (The_Package, In_Tree) /=
                                                      Names.Table (1).Name
                        loop
                           The_Package :=
                             Next_Package_In_Project (The_Package, In_Tree);
                        end loop;

                        --  If it has not been already declared, report an
                        --  error.

                        if No (The_Package) then
                           Error_Msg_Name_1 := Names.Table (1).Name;
                           Error_Msg ("package % not yet defined",
                                      Names.Table (1).Location);
                        end if;
                     end if;

                  else
                     --  It is a project name

                     First_Attribute := Attribute_First;
                     The_Package     := Empty_Node;
                  end if;

               when others =>

                  --  We have either a project name made of several simple
                  --  names (long project), or a project name (short project)
                  --  followed by a package name. The long project name has
                  --  precedence.

                  declare
                     Short_Project : Name_Id;
                     Long_Project  : Name_Id;

                  begin
                     --  Clear the Buffer

                     Buffer_Last := 0;

                     --  Get the name of the short project

                     for Index in 1 .. Names.Last - 1 loop
                        Add_To_Buffer
                          (Get_Name_String (Names.Table (Index).Name),
                           Buffer, Buffer_Last);

                        if Index /= Names.Last - 1 then
                           Add_To_Buffer (".", Buffer, Buffer_Last);
                        end if;
                     end loop;

                     Name_Len := Buffer_Last;
                     Name_Buffer (1 .. Buffer_Last) :=
                       Buffer (1 .. Buffer_Last);
                     Short_Project := Name_Find;

                     --  Now, add the last simple name to get the name of the
                     --  long project.

                     Add_To_Buffer (".", Buffer, Buffer_Last);
                     Add_To_Buffer
                       (Get_Name_String (Names.Table (Names.Last).Name),
                        Buffer, Buffer_Last);
                     Name_Len := Buffer_Last;
                     Name_Buffer (1 .. Buffer_Last) :=
                       Buffer (1 .. Buffer_Last);
                     Long_Project := Name_Find;

                     --  Check if the long project is imported or extended

                     if Long_Project = Name_Of (Current_Project, In_Tree) then
                        The_Project := Current_Project;

                     else
                        The_Project :=
                          Imported_Or_Extended_Project_Of
                            (Current_Project,
                             In_Tree,
                             Long_Project);
                     end if;

                     --  If the long project exists, then this is the prefix
                     --  of the attribute.

                     if Present (The_Project) then
                        First_Attribute := Attribute_First;
                        The_Package     := Empty_Node;

                     else
                        --  Otherwise, check if the short project is imported
                        --  or extended.

                        if Short_Project =
                             Name_Of (Current_Project, In_Tree)
                        then
                           The_Project := Current_Project;

                        else
                           The_Project := Imported_Or_Extended_Project_Of
                                            (Current_Project, In_Tree,
                                             Short_Project);
                        end if;

                        --  If short project does not exist, report an error

                        if No (The_Project) then
                           Error_Msg_Name_1 := Long_Project;
                           Error_Msg_Name_2 := Short_Project;
                           Error_Msg ("unknown projects % or %",
                                      Names.Table (1).Location);
                           The_Package := Empty_Node;
                           First_Attribute := Attribute_First;

                        else
                           --  Now, we check if the package has been declared
                           --  in this project.

                           The_Package :=
                             First_Package_Of (The_Project, In_Tree);
                           while Present (The_Package)
                             and then Name_Of (The_Package, In_Tree) /=
                             Names.Table (Names.Last).Name
                           loop
                              The_Package :=
                                Next_Package_In_Project (The_Package, In_Tree);
                           end loop;

                           --  If it has not, then we report an error

                           if No (The_Package) then
                              Error_Msg_Name_1 :=
                                Names.Table (Names.Last).Name;
                              Error_Msg_Name_2 := Short_Project;
                              Error_Msg ("package % not declared in project %",
                                         Names.Table (Names.Last).Location);
                              First_Attribute := Attribute_First;

                           else
                              --  Otherwise, we have the correct project and
                              --  package.

                              First_Attribute :=
                                First_Attribute_Of
                                  (Package_Id_Of (The_Package, In_Tree));
                           end if;
                        end if;
                     end if;
                  end;
            end case;

            Attribute_Reference
              (In_Tree,
               Variable,
               Current_Project => The_Project,
               Current_Package => The_Package,
               First_Attribute => First_Attribute);
            return;
         end if;
      end if;

      Variable :=
        Default_Project_Node
          (Of_Kind => N_Variable_Reference, In_Tree => In_Tree);

      if Look_For_Variable then
         case Names.Last is
            when 0 =>

               --  Cannot happen (so why null instead of raise PE???)

               null;

            when 1 =>

               --  Simple variable name

               Set_Name_Of (Variable, In_Tree, To => Names.Table (1).Name);

            when 2 =>

               --  Variable name with a simple name prefix that can be
               --  a project name or a package name. Project names have
               --  priority over package names.

               Set_Name_Of (Variable, In_Tree, To => Names.Table (2).Name);

               --  Check if it can be a package name

               The_Package := First_Package_Of (Current_Project, In_Tree);

               while Present (The_Package)
                 and then Name_Of (The_Package, In_Tree) /=
                            Names.Table (1).Name
               loop
                  The_Package :=
                    Next_Package_In_Project (The_Package, In_Tree);
               end loop;

               --  Now look for a possible project name

               The_Project := Imported_Or_Extended_Project_Of
                              (Current_Project, In_Tree, Names.Table (1).Name);

               if Present (The_Project) then
                  Specified_Project := The_Project;

               elsif No (The_Package) then
                  Error_Msg_Name_1 := Names.Table (1).Name;
                  Error_Msg ("unknown package or project %",
                             Names.Table (1).Location);
                  Look_For_Variable := False;

               else
                  Specified_Package := The_Package;
               end if;

            when others =>

               --  Variable name with a prefix that is either a project name
               --  made of several simple names, or a project name followed
               --  by a package name.

               Set_Name_Of
                 (Variable, In_Tree, To => Names.Table (Names.Last).Name);

               declare
                  Short_Project : Name_Id;
                  Long_Project  : Name_Id;

               begin
                  --  First, we get the two possible project names

                  --  Clear the buffer

                  Buffer_Last := 0;

                  --  Add all the simple names, except the last two

                  for Index in 1 .. Names.Last - 2 loop
                     Add_To_Buffer
                       (Get_Name_String (Names.Table (Index).Name),
                        Buffer, Buffer_Last);

                     if Index /= Names.Last - 2 then
                        Add_To_Buffer (".", Buffer, Buffer_Last);
                     end if;
                  end loop;

                  Name_Len := Buffer_Last;
                  Name_Buffer (1 .. Name_Len) := Buffer (1 .. Buffer_Last);
                  Short_Project := Name_Find;

                  --  Add the simple name before the name of the variable

                  Add_To_Buffer (".", Buffer, Buffer_Last);
                  Add_To_Buffer
                    (Get_Name_String (Names.Table (Names.Last - 1).Name),
                     Buffer, Buffer_Last);
                  Name_Len := Buffer_Last;
                  Name_Buffer (1 .. Name_Len) := Buffer (1 .. Buffer_Last);
                  Long_Project := Name_Find;

                  --  Check if the prefix is the name of an imported or
                  --  extended project.

                  The_Project := Imported_Or_Extended_Project_Of
                                   (Current_Project, In_Tree, Long_Project);

                  if Present (The_Project) then
                     Specified_Project := The_Project;

                  else
                     --  Now check if the prefix may be a project name followed
                     --  by a package name.

                     --  First check for a possible project name

                     The_Project :=
                       Imported_Or_Extended_Project_Of
                         (Current_Project, In_Tree, Short_Project);

                     if No (The_Project) then
                        --  Unknown prefix, report an error

                        Error_Msg_Name_1 := Long_Project;
                        Error_Msg_Name_2 := Short_Project;
                        Error_Msg
                          ("unknown projects % or %",
                           Names.Table (1).Location);
                        Look_For_Variable := False;

                     else
                        Specified_Project := The_Project;

                        --  Now look for the package in this project

                        The_Package := First_Package_Of (The_Project, In_Tree);

                        while Present (The_Package)
                          and then Name_Of (The_Package, In_Tree) /=
                                              Names.Table (Names.Last - 1).Name
                        loop
                           The_Package :=
                             Next_Package_In_Project (The_Package, In_Tree);
                        end loop;

                        if No (The_Package) then

                           --  The package does not exist, report an error

                           Error_Msg_Name_1 := Names.Table (2).Name;
                           Error_Msg ("unknown package %",
                                   Names.Table (Names.Last - 1).Location);
                           Look_For_Variable := False;

                        else
                           Specified_Package := The_Package;
                        end if;
                     end if;
                  end if;
               end;
         end case;
      end if;

      if Look_For_Variable then
         Variable_Name := Name_Of (Variable, In_Tree);
         Set_Project_Node_Of (Variable, In_Tree, To => Specified_Project);
         Set_Package_Node_Of (Variable, In_Tree, To => Specified_Package);

         if Present (Specified_Project) then
            The_Project := Specified_Project;
         else
            The_Project := Current_Project;
         end if;

         Current_Variable := Empty_Node;

         --  Look for this variable

         --  If a package was specified, check if the variable has been
         --  declared in this package.

         if Present (Specified_Package) then
            Current_Variable :=
              First_Variable_Of (Specified_Package, In_Tree);
            while Present (Current_Variable)
              and then
              Name_Of (Current_Variable, In_Tree) /= Variable_Name
            loop
               Current_Variable := Next_Variable (Current_Variable, In_Tree);
            end loop;

         else
            --  Otherwise, if no project has been specified and we are in
            --  a package, first check if the variable has been declared in
            --  the package.

            if No (Specified_Project)
              and then Present (Current_Package)
            then
               Current_Variable :=
                 First_Variable_Of (Current_Package, In_Tree);
               while Present (Current_Variable)
                 and then Name_Of (Current_Variable, In_Tree) /= Variable_Name
               loop
                  Current_Variable :=
                    Next_Variable (Current_Variable, In_Tree);
               end loop;
            end if;

            --  If we have not found the variable in the package, check if the
            --  variable has been declared in the project, or in any of its
            --  ancestors.

            if No (Current_Variable) then
               declare
                  Proj : Project_Node_Id := The_Project;

               begin
                  loop
                     Current_Variable := First_Variable_Of (Proj, In_Tree);
                     while
                       Present (Current_Variable)
                       and then
                       Name_Of (Current_Variable, In_Tree) /= Variable_Name
                     loop
                        Current_Variable :=
                          Next_Variable (Current_Variable, In_Tree);
                     end loop;

                     exit when Present (Current_Variable);

                     Proj := Parent_Project_Of (Proj, In_Tree);

                     Set_Project_Node_Of (Variable, In_Tree, To => Proj);

                     exit when No (Proj);
                  end loop;
               end;
            end if;
         end if;

         --  If the variable was not found, report an error

         if No (Current_Variable) then
            Error_Msg_Name_1 := Variable_Name;
            Error_Msg
              ("unknown variable %", Names.Table (Names.Last).Location);
         end if;
      end if;

      if Present (Current_Variable) then
         Set_Expression_Kind_Of
           (Variable, In_Tree,
            To => Expression_Kind_Of (Current_Variable, In_Tree));

         if Kind_Of (Current_Variable, In_Tree) =
                                      N_Typed_Variable_Declaration
         then
            Set_String_Type_Of
              (Variable, In_Tree,
               To => String_Type_Of (Current_Variable, In_Tree));
         end if;
      end if;

      --  If the variable is followed by a left parenthesis, report an error
      --  but attempt to scan the index.

      if Token = Tok_Left_Paren then
         Error_Msg ("\variables cannot be associative arrays", Token_Ptr);
         Scan (In_Tree);
         Expect (Tok_String_Literal, "literal string");

         if Token = Tok_String_Literal then
            Scan (In_Tree);
            Expect (Tok_Right_Paren, "`)`");

            if Token = Tok_Right_Paren then
               Scan (In_Tree);
            end if;
         end if;
      end if;
   end Parse_Variable_Reference;

   ---------------------------------
   -- Start_New_Case_Construction --
   ---------------------------------

   procedure Start_New_Case_Construction
     (In_Tree      : Project_Node_Tree_Ref;
      String_Type  : Project_Node_Id)
   is
      Current_String : Project_Node_Id;

   begin
      --  Set Choice_First, depending on whether this is the first case
      --  construction or not.

      if Choice_First = 0 then
         Choice_First := 1;
         Choices.Set_Last (First_Choice_Node_Id);
      else
         Choice_First := Choices.Last + 1;
      end if;

      --  Add the literal of the string type to the Choices table

      if Present (String_Type) then
         Current_String := First_Literal_String (String_Type, In_Tree);
         while Present (Current_String) loop
            Add (This_String => String_Value_Of (Current_String, In_Tree));
            Current_String := Next_Literal_String (Current_String, In_Tree);
         end loop;
      end if;

      --  Set the value of the last choice in table Choice_Lasts

      Choice_Lasts.Increment_Last;
      Choice_Lasts.Table (Choice_Lasts.Last) := Choices.Last;
   end Start_New_Case_Construction;

   -----------
   -- Terms --
   -----------

   procedure Terms
     (In_Tree         : Project_Node_Tree_Ref;
      Term            : out Project_Node_Id;
      Expr_Kind       : in out Variable_Kind;
      Current_Project : Project_Node_Id;
      Current_Package : Project_Node_Id;
      Optional_Index  : Boolean)
   is
      Next_Term          : Project_Node_Id := Empty_Node;
      Term_Id            : Project_Node_Id := Empty_Node;
      Current_Expression : Project_Node_Id := Empty_Node;
      Next_Expression    : Project_Node_Id := Empty_Node;
      Current_Location   : Source_Ptr      := No_Location;
      Reference          : Project_Node_Id := Empty_Node;

   begin
      --  Declare a new node for the term

      Term := Default_Project_Node (Of_Kind => N_Term, In_Tree => In_Tree);
      Set_Location_Of (Term, In_Tree, To => Token_Ptr);

      case Token is
         when Tok_Left_Paren =>

            --  If we have a left parenthesis and we don't know the expression
            --  kind, then this is a string list.

            case Expr_Kind is
               when Undefined =>
                  Expr_Kind := List;

               when List =>
                  null;

               when Single =>

                  --  If we already know that this is a single string, report
                  --  an error, but set the expression kind to string list to
                  --  avoid several errors.

                  Expr_Kind := List;
                  Error_Msg
                    ("literal string list cannot appear in a string",
                     Token_Ptr);
            end case;

            --  Declare a new node for this literal string list

            Term_Id := Default_Project_Node
              (Of_Kind       => N_Literal_String_List,
               In_Tree       => In_Tree,
               And_Expr_Kind => List);
            Set_Current_Term (Term, In_Tree, To => Term_Id);
            Set_Location_Of  (Term, In_Tree, To => Token_Ptr);

            --  Scan past the left parenthesis

            Scan (In_Tree);

            --  If the left parenthesis is immediately followed by a right
            --  parenthesis, the literal string list is empty.

            if Token = Tok_Right_Paren then
               Scan (In_Tree);

            else
               --  Otherwise parse the expression(s) in the literal string list

               loop
                  Current_Location := Token_Ptr;
                  Parse_Expression
                    (In_Tree         => In_Tree,
                     Expression      => Next_Expression,
                     Current_Project => Current_Project,
                     Current_Package => Current_Package,
                     Optional_Index  => Optional_Index);

                  --  The expression kind is String list, report an error

                  if Expression_Kind_Of (Next_Expression, In_Tree) = List then
                     Error_Msg ("single expression expected",
                                Current_Location);
                  end if;

                  --  If Current_Expression is empty, it means that the
                  --  expression is the first in the string list.

                  if No (Current_Expression) then
                     Set_First_Expression_In_List
                       (Term_Id, In_Tree, To => Next_Expression);
                  else
                     Set_Next_Expression_In_List
                       (Current_Expression, In_Tree, To => Next_Expression);
                  end if;

                  Current_Expression := Next_Expression;

                  --  If there is a comma, continue with the next expression

                  exit when Token /= Tok_Comma;
                  Scan (In_Tree); -- past the comma
               end loop;

               --  We expect a closing right parenthesis

               Expect (Tok_Right_Paren, "`)`");

               if Token = Tok_Right_Paren then
                  Scan (In_Tree);
               end if;
            end if;

         when Tok_String_Literal =>

            --  If we don't know the expression kind (first term), then it is
            --  a simple string.

            if Expr_Kind = Undefined then
               Expr_Kind := Single;
            end if;

            --  Declare a new node for the string literal

            Term_Id :=
              Default_Project_Node
                (Of_Kind => N_Literal_String, In_Tree => In_Tree);
            Set_Current_Term (Term, In_Tree, To => Term_Id);
            Set_String_Value_Of (Term_Id, In_Tree, To => Token_Name);

            --  Scan past the string literal

            Scan (In_Tree);

            --  Check for possible index expression

            if Token = Tok_At then
               if not Optional_Index then
                  Error_Msg ("index not allowed here", Token_Ptr);
                  Scan (In_Tree);

                  if Token = Tok_Integer_Literal then
                     Scan (In_Tree);
                  end if;

               --  Set the index value

               else
                  Scan (In_Tree);
                  Expect (Tok_Integer_Literal, "integer literal");

                  if Token = Tok_Integer_Literal then
                     declare
                        Index : constant Int := UI_To_Int (Int_Literal_Value);
                     begin
                        if Index = 0 then
                           Error_Msg ("index cannot be zero", Token_Ptr);
                        else
                           Set_Source_Index_Of
                             (Term_Id, In_Tree, To => Index);
                        end if;
                     end;

                     Scan (In_Tree);
                  end if;
               end if;
            end if;

         when Tok_Identifier =>
            Current_Location := Token_Ptr;

            --  Get the variable or attribute reference

            Parse_Variable_Reference
              (In_Tree         => In_Tree,
               Variable        => Reference,
               Current_Project => Current_Project,
               Current_Package => Current_Package);
            Set_Current_Term (Term, In_Tree, To => Reference);

            if Present (Reference) then

               --  If we don't know the expression kind (first term), then it
               --  has the kind of the variable or attribute reference.

               if Expr_Kind = Undefined then
                  Expr_Kind := Expression_Kind_Of (Reference, In_Tree);

               elsif Expr_Kind = Single
                 and then Expression_Kind_Of (Reference, In_Tree) = List
               then
                  --  If the expression is a single list, and the reference is
                  --  a string list, report an error, and set the expression
                  --  kind to string list to avoid multiple errors.

                  Expr_Kind := List;
                  Error_Msg
                    ("list variable cannot appear in single string expression",
                     Current_Location);
               end if;
            end if;

         when Tok_Project =>

            --  Project can appear in an expression as the prefix of an
            --  attribute reference of the current project.

            Current_Location := Token_Ptr;
            Scan (In_Tree);
            Expect (Tok_Apostrophe, "`'`");

            if Token = Tok_Apostrophe then
               Attribute_Reference
                 (In_Tree         => In_Tree,
                  Reference       => Reference,
                  First_Attribute => Prj.Attr.Attribute_First,
                  Current_Project => Current_Project,
                  Current_Package => Empty_Node);
               Set_Current_Term (Term, In_Tree, To => Reference);
            end if;

            --  Same checks as above for the expression kind

            if Present (Reference) then
               if Expr_Kind = Undefined then
                  Expr_Kind := Expression_Kind_Of (Reference, In_Tree);

               elsif Expr_Kind = Single
                 and then Expression_Kind_Of (Reference, In_Tree) = List
               then
                  Error_Msg
                    ("lists cannot appear in single string expression",
                     Current_Location);
               end if;
            end if;

         when Tok_External =>

            --  An external reference is always a single string

            if Expr_Kind = Undefined then
               Expr_Kind := Single;
            end if;

            External_Reference
              (In_Tree         => In_Tree,
               Current_Project => Current_Project,
               Current_Package => Current_Package,
               External_Value  => Reference);
            Set_Current_Term (Term, In_Tree, To => Reference);

         when others =>
            Error_Msg ("cannot be part of an expression", Token_Ptr);
            Term := Empty_Node;
            return;
      end case;

      --  If there is an '&', call Terms recursively

      if Token = Tok_Ampersand then
         Scan (In_Tree); -- scan past ampersand

         Terms
           (In_Tree         => In_Tree,
            Term            => Next_Term,
            Expr_Kind       => Expr_Kind,
            Current_Project => Current_Project,
            Current_Package => Current_Package,
            Optional_Index  => Optional_Index);

         --  And link the next term to this term

         Set_Next_Term (Term, In_Tree, To => Next_Term);
      end if;
   end Terms;

end Prj.Strt;
