------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . D E C T                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2001-2010, Free Software Foundation, Inc.         --
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

with GNAT.Case_Util;        use GNAT.Case_Util;
with GNAT.Spelling_Checker; use GNAT.Spelling_Checker;

with Opt;         use Opt;
with Prj.Attr;    use Prj.Attr;
with Prj.Attr.PM; use Prj.Attr.PM;
with Prj.Err;     use Prj.Err;
with Prj.Strt;    use Prj.Strt;
with Prj.Tree;    use Prj.Tree;
with Snames;
with Uintp;       use Uintp;

with GNAT.Strings;

package body Prj.Dect is

   use GNAT;

   type Zone is (In_Project, In_Package, In_Case_Construction);
   --  Used to indicate if we are parsing a package (In_Package),
   --  a case construction (In_Case_Construction) or none of those two
   --  (In_Project).

   procedure Parse_Attribute_Declaration
     (In_Tree           : Project_Node_Tree_Ref;
      Attribute         : out Project_Node_Id;
      First_Attribute   : Attribute_Node_Id;
      Current_Project   : Project_Node_Id;
      Current_Package   : Project_Node_Id;
      Packages_To_Check : String_List_Access;
      Flags             : Processing_Flags);
   --  Parse an attribute declaration

   procedure Parse_Case_Construction
     (In_Tree           : Project_Node_Tree_Ref;
      Case_Construction : out Project_Node_Id;
      First_Attribute   : Attribute_Node_Id;
      Current_Project   : Project_Node_Id;
      Current_Package   : Project_Node_Id;
      Packages_To_Check : String_List_Access;
      Is_Config_File    : Boolean;
      Flags             : Processing_Flags);
   --  Parse a case construction

   procedure Parse_Declarative_Items
     (In_Tree           : Project_Node_Tree_Ref;
      Declarations      : out Project_Node_Id;
      In_Zone           : Zone;
      First_Attribute   : Attribute_Node_Id;
      Current_Project   : Project_Node_Id;
      Current_Package   : Project_Node_Id;
      Packages_To_Check : String_List_Access;
      Is_Config_File    : Boolean;
      Flags             : Processing_Flags);
   --  Parse declarative items. Depending on In_Zone, some declarative items
   --  may be forbidden. Is_Config_File should be set to True if the project
   --  represents a config file (.cgpr) since some specific checks apply.

   procedure Parse_Package_Declaration
     (In_Tree             : Project_Node_Tree_Ref;
      Package_Declaration : out Project_Node_Id;
      Current_Project     : Project_Node_Id;
      Packages_To_Check   : String_List_Access;
      Is_Config_File      : Boolean;
      Flags               : Processing_Flags);
   --  Parse a package declaration.
   --  Is_Config_File should be set to True if the project represents a config
   --  file (.cgpr) since some specific checks apply.

   procedure Parse_String_Type_Declaration
     (In_Tree         : Project_Node_Tree_Ref;
      String_Type     : out Project_Node_Id;
      Current_Project : Project_Node_Id;
      Flags           : Processing_Flags);
   --  type <name> is ( <literal_string> { , <literal_string> } ) ;

   procedure Parse_Variable_Declaration
     (In_Tree         : Project_Node_Tree_Ref;
      Variable        : out Project_Node_Id;
      Current_Project : Project_Node_Id;
      Current_Package : Project_Node_Id;
      Flags           : Processing_Flags);
   --  Parse a variable assignment
   --  <variable_Name> := <expression>; OR
   --  <variable_Name> : <string_type_Name> := <string_expression>;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (In_Tree           : Project_Node_Tree_Ref;
      Declarations      : out Project_Node_Id;
      Current_Project   : Project_Node_Id;
      Extends           : Project_Node_Id;
      Packages_To_Check : String_List_Access;
      Is_Config_File    : Boolean;
      Flags             : Processing_Flags)
   is
      First_Declarative_Item : Project_Node_Id := Empty_Node;

   begin
      Declarations :=
        Default_Project_Node
          (Of_Kind => N_Project_Declaration, In_Tree => In_Tree);
      Set_Location_Of (Declarations, In_Tree, To => Token_Ptr);
      Set_Extended_Project_Of (Declarations, In_Tree, To => Extends);
      Set_Project_Declaration_Of (Current_Project, In_Tree, Declarations);
      Parse_Declarative_Items
        (Declarations      => First_Declarative_Item,
         In_Tree           => In_Tree,
         In_Zone           => In_Project,
         First_Attribute   => Prj.Attr.Attribute_First,
         Current_Project   => Current_Project,
         Current_Package   => Empty_Node,
         Packages_To_Check => Packages_To_Check,
         Is_Config_File    => Is_Config_File,
         Flags             => Flags);
      Set_First_Declarative_Item_Of
        (Declarations, In_Tree, To => First_Declarative_Item);
   end Parse;

   ---------------------------------
   -- Parse_Attribute_Declaration --
   ---------------------------------

   procedure Parse_Attribute_Declaration
     (In_Tree           : Project_Node_Tree_Ref;
      Attribute         : out Project_Node_Id;
      First_Attribute   : Attribute_Node_Id;
      Current_Project   : Project_Node_Id;
      Current_Package   : Project_Node_Id;
      Packages_To_Check : String_List_Access;
      Flags             : Processing_Flags)
   is
      Current_Attribute      : Attribute_Node_Id := First_Attribute;
      Full_Associative_Array : Boolean           := False;
      Attribute_Name         : Name_Id           := No_Name;
      Optional_Index         : Boolean           := False;
      Pkg_Id                 : Package_Node_Id   := Empty_Package;
      Ignore                 : Boolean           := False;

   begin
      Attribute :=
        Default_Project_Node
          (Of_Kind => N_Attribute_Declaration, In_Tree => In_Tree);
      Set_Location_Of (Attribute, In_Tree, To => Token_Ptr);
      Set_Previous_Line_Node (Attribute);

      --  Scan past "for"

      Scan (In_Tree);

      --  Body may be an attribute name

      if Token = Tok_Body then
         Token := Tok_Identifier;
         Token_Name := Snames.Name_Body;
      end if;

      Expect (Tok_Identifier, "identifier");

      if Token = Tok_Identifier then
         Attribute_Name := Token_Name;
         Set_Name_Of (Attribute, In_Tree, To => Token_Name);
         Set_Location_Of (Attribute, In_Tree, To => Token_Ptr);

         --  Find the attribute

         Current_Attribute :=
           Attribute_Node_Id_Of (Token_Name, First_Attribute);

         --  If the attribute cannot be found, create the attribute if inside
         --  an unknown package.

         if Current_Attribute = Empty_Attribute then
            if Present (Current_Package)
              and then Expression_Kind_Of (Current_Package, In_Tree) = Ignored
            then
               Pkg_Id := Package_Id_Of (Current_Package, In_Tree);
               Add_Attribute (Pkg_Id, Token_Name, Current_Attribute);

            else
               --  If not a valid attribute name, issue an error if inside
               --  a package that need to be checked.

               Ignore := Present (Current_Package) and then
                          Packages_To_Check /= All_Packages;

               if Ignore then

                  --  Check that we are not in a package to check

                  Get_Name_String (Name_Of (Current_Package, In_Tree));

                  for Index in Packages_To_Check'Range loop
                     if Name_Buffer (1 .. Name_Len) =
                       Packages_To_Check (Index).all
                     then
                        Ignore := False;
                        exit;
                     end if;
                  end loop;
               end if;

               if not Ignore then
                  Error_Msg_Name_1 := Token_Name;
                  Error_Msg (Flags, "undefined attribute %%", Token_Ptr);
               end if;
            end if;

         --  Set, if appropriate the index case insensitivity flag

         else
            if Is_Read_Only (Current_Attribute) then
               Error_Msg_Name_1 := Token_Name;
               Error_Msg
                 (Flags, "read-only attribute %% cannot be given a value",
                  Token_Ptr);
            end if;

            if Attribute_Kind_Of (Current_Attribute) in
                 All_Case_Insensitive_Associative_Array
            then
               Set_Case_Insensitive (Attribute, In_Tree, To => True);
            end if;
         end if;

         Scan (In_Tree); --  past the attribute name
      end if;

      --  Change obsolete names of attributes to the new names

      if Present (Current_Package)
        and then Expression_Kind_Of (Current_Package, In_Tree) /= Ignored
      then
         case Name_Of (Attribute, In_Tree) is
         when Snames.Name_Specification =>
            Set_Name_Of (Attribute, In_Tree, To => Snames.Name_Spec);

         when Snames.Name_Specification_Suffix =>
            Set_Name_Of (Attribute, In_Tree, To => Snames.Name_Spec_Suffix);

         when Snames.Name_Implementation =>
            Set_Name_Of (Attribute, In_Tree, To => Snames.Name_Body);

         when Snames.Name_Implementation_Suffix =>
            Set_Name_Of (Attribute, In_Tree, To => Snames.Name_Body_Suffix);

         when others =>
            null;
         end case;
      end if;

      --  Associative array attributes

      if Token = Tok_Left_Paren then

         --  If the attribute is not an associative array attribute, report
         --  an error. If this information is still unknown, set the kind
         --  to Associative_Array.

         if Current_Attribute /= Empty_Attribute
           and then Attribute_Kind_Of (Current_Attribute) = Single
         then
            Error_Msg (Flags,
                       "the attribute """ &
                       Get_Name_String
                          (Attribute_Name_Of (Current_Attribute)) &
                       """ cannot be an associative array",
                       Location_Of (Attribute, In_Tree));

         elsif Attribute_Kind_Of (Current_Attribute) = Unknown then
            Set_Attribute_Kind_Of (Current_Attribute, To => Associative_Array);
         end if;

         Scan (In_Tree); --  past the left parenthesis

         if Others_Allowed_For (Current_Attribute)
           and then Token = Tok_Others
         then
            Set_Associative_Array_Index_Of
              (Attribute, In_Tree, All_Other_Names);
            Scan (In_Tree); --  past others

         else
            if Others_Allowed_For (Current_Attribute) then
               Expect (Tok_String_Literal, "literal string or others");
            else
               Expect (Tok_String_Literal, "literal string");
            end if;

            if Token = Tok_String_Literal then
               Get_Name_String (Token_Name);

               if Case_Insensitive (Attribute, In_Tree) then
                  To_Lower (Name_Buffer (1 .. Name_Len));
               end if;

               Set_Associative_Array_Index_Of (Attribute, In_Tree, Name_Find);
               Scan (In_Tree); --  past the literal string index

               if Token = Tok_At then
                  case Attribute_Kind_Of (Current_Attribute) is
                  when Optional_Index_Associative_Array |
                       Optional_Index_Case_Insensitive_Associative_Array =>
                     Scan (In_Tree);
                     Expect (Tok_Integer_Literal, "integer literal");

                     if Token = Tok_Integer_Literal then

                        --  Set the source index value from given literal

                        declare
                           Index : constant Int :=
                                     UI_To_Int (Int_Literal_Value);
                        begin
                           if Index = 0 then
                              Error_Msg
                                (Flags, "index cannot be zero", Token_Ptr);
                           else
                              Set_Source_Index_Of
                                (Attribute, In_Tree, To => Index);
                           end if;
                        end;

                        Scan (In_Tree);
                     end if;

                  when others =>
                     Error_Msg (Flags, "index not allowed here", Token_Ptr);
                     Scan (In_Tree);

                     if Token = Tok_Integer_Literal then
                        Scan (In_Tree);
                     end if;
                  end case;
               end if;
            end if;
         end if;

         Expect (Tok_Right_Paren, "`)`");

         if Token = Tok_Right_Paren then
            Scan (In_Tree); --  past the right parenthesis
         end if;

      else
         --  If it is an associative array attribute and there are no left
         --  parenthesis, then this is a full associative array declaration.
         --  Flag it as such for later processing of its value.

         if Current_Attribute /= Empty_Attribute
           and then
             Attribute_Kind_Of (Current_Attribute) /= Single
         then
            if Attribute_Kind_Of (Current_Attribute) = Unknown then
               Set_Attribute_Kind_Of (Current_Attribute, To => Single);

            else
               Full_Associative_Array := True;
            end if;
         end if;
      end if;

      --  Set the expression kind of the attribute

      if Current_Attribute /= Empty_Attribute then
         Set_Expression_Kind_Of
           (Attribute, In_Tree, To => Variable_Kind_Of (Current_Attribute));
         Optional_Index := Optional_Index_Of (Current_Attribute);
      end if;

      Expect (Tok_Use, "USE");

      if Token = Tok_Use then
         Scan (In_Tree);

         if Full_Associative_Array then

            --  Expect <project>'<same_attribute_name>, or
            --  <project>.<same_package_name>'<same_attribute_name>

            declare
               The_Project : Project_Node_Id := Empty_Node;
               --  The node of the project where the associative array is
               --  declared.

               The_Package : Project_Node_Id := Empty_Node;
               --  The node of the package where the associative array is
               --  declared, if any.

               Project_Name : Name_Id := No_Name;
               --  The name of the project where the associative array is
               --  declared.

               Location : Source_Ptr := No_Location;
               --  The location of the project name

            begin
               Expect (Tok_Identifier, "identifier");

               if Token = Tok_Identifier then
                  Location := Token_Ptr;

                  --  Find the project node in the imported project or
                  --  in the project being extended.

                  The_Project := Imported_Or_Extended_Project_Of
                                   (Current_Project, In_Tree, Token_Name);

                  if No (The_Project) then
                     Error_Msg (Flags, "unknown project", Location);
                     Scan (In_Tree); --  past the project name

                  else
                     Project_Name := Token_Name;
                     Scan (In_Tree); --  past the project name

                     --  If this is inside a package, a dot followed by the
                     --  name of the package must followed the project name.

                     if Present (Current_Package) then
                        Expect (Tok_Dot, "`.`");

                        if Token /= Tok_Dot then
                           The_Project := Empty_Node;

                        else
                           Scan (In_Tree); --  past the dot
                           Expect (Tok_Identifier, "identifier");

                           if Token /= Tok_Identifier then
                              The_Project := Empty_Node;

                           --  If it is not the same package name, issue error

                           elsif
                             Token_Name /= Name_Of (Current_Package, In_Tree)
                           then
                              The_Project := Empty_Node;
                              Error_Msg
                                (Flags, "not the same package as " &
                                 Get_Name_String
                                   (Name_Of (Current_Package, In_Tree)),
                                 Token_Ptr);

                           else
                              The_Package :=
                                First_Package_Of (The_Project, In_Tree);

                              --  Look for the package node

                              while Present (The_Package)
                                and then
                                Name_Of (The_Package, In_Tree) /= Token_Name
                              loop
                                 The_Package :=
                                   Next_Package_In_Project
                                     (The_Package, In_Tree);
                              end loop;

                              --  If the package cannot be found in the
                              --  project, issue an error.

                              if No (The_Package) then
                                 The_Project := Empty_Node;
                                 Error_Msg_Name_2 := Project_Name;
                                 Error_Msg_Name_1 := Token_Name;
                                 Error_Msg
                                   (Flags,
                                    "package % not declared in project %",
                                    Token_Ptr);
                              end if;

                              Scan (In_Tree); --  past the package name
                           end if;
                        end if;
                     end if;
                  end if;
               end if;

               if Present (The_Project) then

                  --  Looking for '<same attribute name>

                  Expect (Tok_Apostrophe, "`''`");

                  if Token /= Tok_Apostrophe then
                     The_Project := Empty_Node;

                  else
                     Scan (In_Tree); --  past the apostrophe
                     Expect (Tok_Identifier, "identifier");

                     if Token /= Tok_Identifier then
                        The_Project := Empty_Node;

                     else
                        --  If it is not the same attribute name, issue error

                        if Token_Name /= Attribute_Name then
                           The_Project := Empty_Node;
                           Error_Msg_Name_1 := Attribute_Name;
                           Error_Msg
                             (Flags, "invalid name, should be %", Token_Ptr);
                        end if;

                        Scan (In_Tree); --  past the attribute name
                     end if;
                  end if;
               end if;

               if No (The_Project) then

                  --  If there were any problem, set the attribute id to null,
                  --  so that the node will not be recorded.

                  Current_Attribute := Empty_Attribute;

               else
                  --  Set the appropriate field in the node.
                  --  Note that the index and the expression are nil. This
                  --  characterizes full associative array attribute
                  --  declarations.

                  Set_Associative_Project_Of (Attribute, In_Tree, The_Project);
                  Set_Associative_Package_Of (Attribute, In_Tree, The_Package);
               end if;
            end;

         --  Other attribute declarations (not full associative array)

         else
            declare
               Expression_Location : constant Source_Ptr := Token_Ptr;
               --  The location of the first token of the expression

               Expression          : Project_Node_Id     := Empty_Node;
               --  The expression, value for the attribute declaration

            begin
               --  Get the expression value and set it in the attribute node

               Parse_Expression
                 (In_Tree         => In_Tree,
                  Expression      => Expression,
                  Flags           => Flags,
                  Current_Project => Current_Project,
                  Current_Package => Current_Package,
                  Optional_Index  => Optional_Index);
               Set_Expression_Of (Attribute, In_Tree, To => Expression);

               --  If the expression is legal, but not of the right kind
               --  for the attribute, issue an error.

               if Current_Attribute /= Empty_Attribute
                 and then Present (Expression)
                 and then Variable_Kind_Of (Current_Attribute) /=
                 Expression_Kind_Of (Expression, In_Tree)
               then
                  if  Variable_Kind_Of (Current_Attribute) = Undefined then
                     Set_Variable_Kind_Of
                       (Current_Attribute,
                        To => Expression_Kind_Of (Expression, In_Tree));

                  else
                     Error_Msg
                       (Flags, "wrong expression kind for attribute """ &
                        Get_Name_String
                          (Attribute_Name_Of (Current_Attribute)) &
                        """",
                        Expression_Location);
                  end if;
               end if;
            end;
         end if;
      end if;

      --  If the attribute was not recognized, return an empty node.
      --  It may be that it is not in a package to check, and the node will
      --  not be added to the tree.

      if Current_Attribute = Empty_Attribute then
         Attribute := Empty_Node;
      end if;

      Set_End_Of_Line (Attribute);
      Set_Previous_Line_Node (Attribute);
   end Parse_Attribute_Declaration;

   -----------------------------
   -- Parse_Case_Construction --
   -----------------------------

   procedure Parse_Case_Construction
     (In_Tree           : Project_Node_Tree_Ref;
      Case_Construction : out Project_Node_Id;
      First_Attribute   : Attribute_Node_Id;
      Current_Project   : Project_Node_Id;
      Current_Package   : Project_Node_Id;
      Packages_To_Check : String_List_Access;
      Is_Config_File    : Boolean;
      Flags             : Processing_Flags)
   is
      Current_Item    : Project_Node_Id := Empty_Node;
      Next_Item       : Project_Node_Id := Empty_Node;
      First_Case_Item : Boolean := True;

      Variable_Location : Source_Ptr := No_Location;

      String_Type : Project_Node_Id := Empty_Node;

      Case_Variable : Project_Node_Id := Empty_Node;

      First_Declarative_Item : Project_Node_Id := Empty_Node;

      First_Choice           : Project_Node_Id := Empty_Node;

      When_Others            : Boolean := False;
      --  Set to True when there is a "when others =>" clause

   begin
      Case_Construction  :=
        Default_Project_Node
          (Of_Kind => N_Case_Construction, In_Tree => In_Tree);
      Set_Location_Of (Case_Construction, In_Tree, To => Token_Ptr);

      --  Scan past "case"

      Scan (In_Tree);

      --  Get the switch variable

      Expect (Tok_Identifier, "identifier");

      if Token = Tok_Identifier then
         Variable_Location := Token_Ptr;
         Parse_Variable_Reference
           (In_Tree         => In_Tree,
            Variable        => Case_Variable,
            Flags           => Flags,
            Current_Project => Current_Project,
            Current_Package => Current_Package);
         Set_Case_Variable_Reference_Of
           (Case_Construction, In_Tree, To => Case_Variable);

      else
         if Token /= Tok_Is then
            Scan (In_Tree);
         end if;
      end if;

      if Present (Case_Variable) then
         String_Type := String_Type_Of (Case_Variable, In_Tree);

         if No (String_Type) then
            Error_Msg (Flags,
                       "variable """ &
                       Get_Name_String (Name_Of (Case_Variable, In_Tree)) &
                       """ is not typed",
                       Variable_Location);
         end if;
      end if;

      Expect (Tok_Is, "IS");

      if Token = Tok_Is then
         Set_End_Of_Line (Case_Construction);
         Set_Previous_Line_Node (Case_Construction);
         Set_Next_End_Node (Case_Construction);

         --  Scan past "is"

         Scan (In_Tree);
      end if;

      Start_New_Case_Construction (In_Tree, String_Type);

      When_Loop :

      while Token = Tok_When loop

         if First_Case_Item then
            Current_Item :=
              Default_Project_Node
                (Of_Kind => N_Case_Item, In_Tree => In_Tree);
            Set_First_Case_Item_Of
              (Case_Construction, In_Tree, To => Current_Item);
            First_Case_Item := False;

         else
            Next_Item :=
              Default_Project_Node
                (Of_Kind => N_Case_Item, In_Tree => In_Tree);
            Set_Next_Case_Item (Current_Item, In_Tree, To => Next_Item);
            Current_Item := Next_Item;
         end if;

         Set_Location_Of (Current_Item, In_Tree, To => Token_Ptr);

         --  Scan past "when"

         Scan (In_Tree);

         if Token = Tok_Others then
            When_Others := True;

            --  Scan past "others"

            Scan (In_Tree);

            Expect (Tok_Arrow, "`=>`");
            Set_End_Of_Line (Current_Item);
            Set_Previous_Line_Node (Current_Item);

            --  Empty_Node in Field1 of a Case_Item indicates
            --  the "when others =>" branch.

            Set_First_Choice_Of (Current_Item, In_Tree, To => Empty_Node);

            Parse_Declarative_Items
              (In_Tree           => In_Tree,
               Declarations      => First_Declarative_Item,
               In_Zone           => In_Case_Construction,
               First_Attribute   => First_Attribute,
               Current_Project   => Current_Project,
               Current_Package   => Current_Package,
               Packages_To_Check => Packages_To_Check,
               Is_Config_File    => Is_Config_File,
               Flags             => Flags);

            --  "when others =>" must be the last branch, so save the
            --  Case_Item and exit

            Set_First_Declarative_Item_Of
              (Current_Item, In_Tree, To => First_Declarative_Item);
            exit When_Loop;

         else
            Parse_Choice_List
              (In_Tree      => In_Tree,
               First_Choice => First_Choice,
               Flags        => Flags);
            Set_First_Choice_Of (Current_Item, In_Tree, To => First_Choice);

            Expect (Tok_Arrow, "`=>`");
            Set_End_Of_Line (Current_Item);
            Set_Previous_Line_Node (Current_Item);

            Parse_Declarative_Items
              (In_Tree           => In_Tree,
               Declarations      => First_Declarative_Item,
               In_Zone           => In_Case_Construction,
               First_Attribute   => First_Attribute,
               Current_Project   => Current_Project,
               Current_Package   => Current_Package,
               Packages_To_Check => Packages_To_Check,
               Is_Config_File    => Is_Config_File,
               Flags             => Flags);

            Set_First_Declarative_Item_Of
              (Current_Item, In_Tree, To => First_Declarative_Item);

         end if;
      end loop When_Loop;

      End_Case_Construction
        (Check_All_Labels => not When_Others and not Quiet_Output,
         Case_Location    => Location_Of (Case_Construction, In_Tree),
         Flags            => Flags);

      Expect (Tok_End, "`END CASE`");
      Remove_Next_End_Node;

      if Token = Tok_End then

         --  Scan past "end"

         Scan (In_Tree);

         Expect (Tok_Case, "CASE");

      end if;

      --  Scan past "case"

      Scan (In_Tree);

      Expect (Tok_Semicolon, "`;`");
      Set_Previous_End_Node (Case_Construction);

   end Parse_Case_Construction;

   -----------------------------
   -- Parse_Declarative_Items --
   -----------------------------

   procedure Parse_Declarative_Items
     (In_Tree           : Project_Node_Tree_Ref;
      Declarations      : out Project_Node_Id;
      In_Zone           : Zone;
      First_Attribute   : Attribute_Node_Id;
      Current_Project   : Project_Node_Id;
      Current_Package   : Project_Node_Id;
      Packages_To_Check : String_List_Access;
      Is_Config_File    : Boolean;
      Flags             : Processing_Flags)
   is
      Current_Declarative_Item : Project_Node_Id := Empty_Node;
      Next_Declarative_Item    : Project_Node_Id := Empty_Node;
      Current_Declaration      : Project_Node_Id := Empty_Node;
      Item_Location            : Source_Ptr      := No_Location;

   begin
      Declarations := Empty_Node;

      loop
         --  We are always positioned at the token that precedes the first
         --  token of the declarative element. Scan past it.

         Scan (In_Tree);

         Item_Location := Token_Ptr;

         case Token is
            when Tok_Identifier =>

               if In_Zone = In_Case_Construction then

                  --  Check if the variable has already been declared

                  declare
                     The_Variable : Project_Node_Id := Empty_Node;

                  begin
                     if Present (Current_Package) then
                        The_Variable :=
                          First_Variable_Of (Current_Package, In_Tree);
                     elsif Present (Current_Project) then
                        The_Variable :=
                          First_Variable_Of (Current_Project, In_Tree);
                     end if;

                     while Present (The_Variable)
                       and then Name_Of (The_Variable, In_Tree) /=
                                Token_Name
                     loop
                        The_Variable := Next_Variable (The_Variable, In_Tree);
                     end loop;

                     --  It is an error to declare a variable in a case
                     --  construction for the first time.

                     if No (The_Variable) then
                        Error_Msg
                          (Flags,
                           "a variable cannot be declared " &
                           "for the first time here",
                           Token_Ptr);
                     end if;
                  end;
               end if;

               Parse_Variable_Declaration
                 (In_Tree,
                  Current_Declaration,
                  Current_Project => Current_Project,
                  Current_Package => Current_Package,
                  Flags           => Flags);

               Set_End_Of_Line (Current_Declaration);
               Set_Previous_Line_Node (Current_Declaration);

            when Tok_For =>

               Parse_Attribute_Declaration
                 (In_Tree           => In_Tree,
                  Attribute         => Current_Declaration,
                  First_Attribute   => First_Attribute,
                  Current_Project   => Current_Project,
                  Current_Package   => Current_Package,
                  Packages_To_Check => Packages_To_Check,
                  Flags             => Flags);

               Set_End_Of_Line (Current_Declaration);
               Set_Previous_Line_Node (Current_Declaration);

            when Tok_Null =>

               Scan (In_Tree); --  past "null"

            when Tok_Package =>

               --  Package declaration

               if In_Zone /= In_Project then
                  Error_Msg
                    (Flags, "a package cannot be declared here", Token_Ptr);
               end if;

               Parse_Package_Declaration
                 (In_Tree             => In_Tree,
                  Package_Declaration => Current_Declaration,
                  Current_Project     => Current_Project,
                  Packages_To_Check   => Packages_To_Check,
                  Is_Config_File      => Is_Config_File,
                  Flags               => Flags);

               Set_Previous_End_Node (Current_Declaration);

            when Tok_Type =>

               --  Type String Declaration

               if In_Zone /= In_Project then
                  Error_Msg (Flags,
                             "a string type cannot be declared here",
                             Token_Ptr);
               end if;

               Parse_String_Type_Declaration
                 (In_Tree         => In_Tree,
                  String_Type     => Current_Declaration,
                  Current_Project => Current_Project,
                  Flags           => Flags);

               Set_End_Of_Line (Current_Declaration);
               Set_Previous_Line_Node (Current_Declaration);

            when Tok_Case =>

               --  Case construction

               Parse_Case_Construction
                 (In_Tree           => In_Tree,
                  Case_Construction => Current_Declaration,
                  First_Attribute   => First_Attribute,
                  Current_Project   => Current_Project,
                  Current_Package   => Current_Package,
                  Packages_To_Check => Packages_To_Check,
                  Is_Config_File    => Is_Config_File,
                  Flags             => Flags);

               Set_Previous_End_Node (Current_Declaration);

            when others =>
               exit;

               --  We are leaving Parse_Declarative_Items positioned
               --  at the first token after the list of declarative items.
               --  It could be "end" (for a project, a package declaration or
               --  a case construction) or "when" (for a case construction)

         end case;

         Expect (Tok_Semicolon, "`;` after declarative items");

         --  Insert an N_Declarative_Item in the tree, but only if
         --  Current_Declaration is not an empty node.

         if Present (Current_Declaration) then
            if No (Current_Declarative_Item) then
               Current_Declarative_Item :=
                 Default_Project_Node
                   (Of_Kind => N_Declarative_Item, In_Tree => In_Tree);
               Declarations  := Current_Declarative_Item;

            else
               Next_Declarative_Item :=
                 Default_Project_Node
                   (Of_Kind => N_Declarative_Item, In_Tree => In_Tree);
               Set_Next_Declarative_Item
                 (Current_Declarative_Item, In_Tree,
                  To => Next_Declarative_Item);
               Current_Declarative_Item := Next_Declarative_Item;
            end if;

            Set_Current_Item_Node
              (Current_Declarative_Item, In_Tree,
               To => Current_Declaration);
            Set_Location_Of
              (Current_Declarative_Item, In_Tree, To => Item_Location);
         end if;
      end loop;
   end Parse_Declarative_Items;

   -------------------------------
   -- Parse_Package_Declaration --
   -------------------------------

   procedure Parse_Package_Declaration
     (In_Tree             : Project_Node_Tree_Ref;
      Package_Declaration : out Project_Node_Id;
      Current_Project     : Project_Node_Id;
      Packages_To_Check   : String_List_Access;
      Is_Config_File      : Boolean;
      Flags               : Processing_Flags)
   is
      First_Attribute        : Attribute_Node_Id := Empty_Attribute;
      Current_Package        : Package_Node_Id   := Empty_Package;
      First_Declarative_Item : Project_Node_Id   := Empty_Node;
      Package_Location       : constant Source_Ptr := Token_Ptr;
      Renaming               : Boolean := False;
      Extending              : Boolean := False;

   begin
      Package_Declaration :=
        Default_Project_Node
          (Of_Kind => N_Package_Declaration, In_Tree => In_Tree);
      Set_Location_Of (Package_Declaration, In_Tree, To => Package_Location);

      --  Scan past "package"

      Scan (In_Tree);
      Expect (Tok_Identifier, "identifier");

      if Token = Tok_Identifier then
         Set_Name_Of (Package_Declaration, In_Tree, To => Token_Name);

         Current_Package := Package_Node_Id_Of (Token_Name);

         if Current_Package = Empty_Package then
            if not Quiet_Output then
               declare
                  List  : constant Strings.String_List := Package_Name_List;
                  Index : Natural;
                  Name  : constant String := Get_Name_String (Token_Name);

               begin
                  --  Check for possible misspelling of a known package name

                  Index := 0;
                  loop
                     if Index >= List'Last then
                        Index := 0;
                        exit;
                     end if;

                     Index := Index + 1;
                     exit when
                       GNAT.Spelling_Checker.Is_Bad_Spelling_Of
                         (Name, List (Index).all);
                  end loop;

                  --  Issue warning(s) in verbose mode or when a possible
                  --  misspelling has been found.

                  if Verbose_Mode or else Index /= 0 then
                     Error_Msg (Flags,
                                "?""" &
                                Get_Name_String
                                 (Name_Of (Package_Declaration, In_Tree)) &
                                """ is not a known package name",
                                Token_Ptr);
                  end if;

                  if Index /= 0 then
                     Error_Msg -- CODEFIX
                       (Flags,
                        "\?possible misspelling of """ &
                        List (Index).all & """", Token_Ptr);
                  end if;
               end;
            end if;

            --  Set the package declaration to "ignored" so that it is not
            --  processed by Prj.Proc.Process.

            Set_Expression_Kind_Of (Package_Declaration, In_Tree, Ignored);

            --  Add the unknown package in the list of packages

            Add_Unknown_Package (Token_Name, Current_Package);

         elsif Current_Package = Unknown_Package then

            --  Set the package declaration to "ignored" so that it is not
            --  processed by Prj.Proc.Process.

            Set_Expression_Kind_Of (Package_Declaration, In_Tree, Ignored);

         else
            First_Attribute := First_Attribute_Of (Current_Package);
         end if;

         Set_Package_Id_Of
           (Package_Declaration, In_Tree, To => Current_Package);

         declare
            Current : Project_Node_Id :=
                        First_Package_Of (Current_Project, In_Tree);

         begin
            while Present (Current)
              and then Name_Of (Current, In_Tree) /= Token_Name
            loop
               Current := Next_Package_In_Project (Current, In_Tree);
            end loop;

            if Present (Current) then
               Error_Msg
                 (Flags,
                  "package """ &
                  Get_Name_String (Name_Of (Package_Declaration, In_Tree)) &
                  """ is declared twice in the same project",
                  Token_Ptr);

            else
               --  Add the package to the project list

               Set_Next_Package_In_Project
                 (Package_Declaration, In_Tree,
                  To => First_Package_Of (Current_Project, In_Tree));
               Set_First_Package_Of
                 (Current_Project, In_Tree, To => Package_Declaration);
            end if;
         end;

         --  Scan past the package name

         Scan (In_Tree);
      end if;

      if Token = Tok_Renames then
         Renaming := True;
      elsif Token = Tok_Extends then
         Extending := True;
      end if;

      if Renaming or else Extending then
         if Is_Config_File then
            Error_Msg
              (Flags,
               "no package rename or extension in configuration projects",
               Token_Ptr);
         end if;

         --  Scan past "renames" or "extends"

         Scan (In_Tree);

         Expect (Tok_Identifier, "identifier");

         if Token = Tok_Identifier then
            declare
               Project_Name : constant Name_Id := Token_Name;

               Clause       : Project_Node_Id :=
                              First_With_Clause_Of (Current_Project, In_Tree);
               The_Project  : Project_Node_Id := Empty_Node;
               Extended     : constant Project_Node_Id :=
                                Extended_Project_Of
                                  (Project_Declaration_Of
                                    (Current_Project, In_Tree),
                                   In_Tree);
            begin
               while Present (Clause) loop
                  --  Only non limited imported projects may be used in a
                  --  renames declaration.

                  The_Project :=
                    Non_Limited_Project_Node_Of (Clause, In_Tree);
                  exit when Present (The_Project)
                    and then Name_Of (The_Project, In_Tree) = Project_Name;
                  Clause := Next_With_Clause_Of (Clause, In_Tree);
               end loop;

               if No (Clause) then
                  --  As we have not found the project in the imports, we check
                  --  if it's the name of an eventual extended project.

                  if Present (Extended)
                    and then Name_Of (Extended, In_Tree) = Project_Name
                  then
                     Set_Project_Of_Renamed_Package_Of
                       (Package_Declaration, In_Tree, To => Extended);
                  else
                     Error_Msg_Name_1 := Project_Name;
                     Error_Msg
                       (Flags,
                        "% is not an imported or extended project", Token_Ptr);
                  end if;
               else
                  Set_Project_Of_Renamed_Package_Of
                    (Package_Declaration, In_Tree, To => The_Project);
               end if;
            end;

            Scan (In_Tree);
            Expect (Tok_Dot, "`.`");

            if Token = Tok_Dot then
               Scan (In_Tree);
               Expect (Tok_Identifier, "identifier");

               if Token = Tok_Identifier then
                  if Name_Of (Package_Declaration, In_Tree) /= Token_Name then
                     Error_Msg (Flags, "not the same package name", Token_Ptr);
                  elsif
                    Present (Project_Of_Renamed_Package_Of
                               (Package_Declaration, In_Tree))
                  then
                     declare
                        Current : Project_Node_Id :=
                                    First_Package_Of
                                      (Project_Of_Renamed_Package_Of
                                           (Package_Declaration, In_Tree),
                                       In_Tree);

                     begin
                        while Present (Current)
                          and then Name_Of (Current, In_Tree) /= Token_Name
                        loop
                           Current :=
                             Next_Package_In_Project (Current, In_Tree);
                        end loop;

                        if No (Current) then
                           Error_Msg
                             (Flags, """" &
                              Get_Name_String (Token_Name) &
                              """ is not a package declared by the project",
                              Token_Ptr);
                        end if;
                     end;
                  end if;

                  Scan (In_Tree);
               end if;
            end if;
         end if;
      end if;

      if Renaming then
         Expect (Tok_Semicolon, "`;`");
         Set_End_Of_Line (Package_Declaration);
         Set_Previous_Line_Node (Package_Declaration);

      elsif Token = Tok_Is then
         Set_End_Of_Line (Package_Declaration);
         Set_Previous_Line_Node (Package_Declaration);
         Set_Next_End_Node (Package_Declaration);

         Parse_Declarative_Items
           (In_Tree           => In_Tree,
            Declarations      => First_Declarative_Item,
            In_Zone           => In_Package,
            First_Attribute   => First_Attribute,
            Current_Project   => Current_Project,
            Current_Package   => Package_Declaration,
            Packages_To_Check => Packages_To_Check,
            Is_Config_File    => Is_Config_File,
            Flags             => Flags);

         Set_First_Declarative_Item_Of
           (Package_Declaration, In_Tree, To => First_Declarative_Item);

         Expect (Tok_End, "END");

         if Token = Tok_End then

            --  Scan past "end"

            Scan (In_Tree);
         end if;

         --  We should have the name of the package after "end"

         Expect (Tok_Identifier, "identifier");

         if Token = Tok_Identifier
           and then Name_Of (Package_Declaration, In_Tree) /= No_Name
           and then Token_Name /= Name_Of (Package_Declaration, In_Tree)
         then
            Error_Msg_Name_1 := Name_Of (Package_Declaration, In_Tree);
            Error_Msg (Flags, "expected %%", Token_Ptr);
         end if;

         if Token /= Tok_Semicolon then

            --  Scan past the package name

            Scan (In_Tree);
         end if;

         Expect (Tok_Semicolon, "`;`");
         Remove_Next_End_Node;

      else
         Error_Msg (Flags, "expected IS", Token_Ptr);
      end if;

   end Parse_Package_Declaration;

   -----------------------------------
   -- Parse_String_Type_Declaration --
   -----------------------------------

   procedure Parse_String_Type_Declaration
     (In_Tree         : Project_Node_Tree_Ref;
      String_Type     : out Project_Node_Id;
      Current_Project : Project_Node_Id;
      Flags           : Processing_Flags)
   is
      Current      : Project_Node_Id := Empty_Node;
      First_String : Project_Node_Id := Empty_Node;

   begin
      String_Type :=
        Default_Project_Node
          (Of_Kind => N_String_Type_Declaration, In_Tree => In_Tree);

      Set_Location_Of (String_Type, In_Tree, To => Token_Ptr);

      --  Scan past "type"

      Scan (In_Tree);

      Expect (Tok_Identifier, "identifier");

      if Token = Tok_Identifier then
         Set_Name_Of (String_Type, In_Tree, To => Token_Name);

         Current := First_String_Type_Of (Current_Project, In_Tree);
         while Present (Current)
           and then
           Name_Of (Current, In_Tree) /= Token_Name
         loop
            Current := Next_String_Type (Current, In_Tree);
         end loop;

         if Present (Current) then
            Error_Msg (Flags,
                       "duplicate string type name """ &
                       Get_Name_String (Token_Name) &
                       """",
                       Token_Ptr);
         else
            Current := First_Variable_Of (Current_Project, In_Tree);
            while Present (Current)
              and then Name_Of (Current, In_Tree) /= Token_Name
            loop
               Current := Next_Variable (Current, In_Tree);
            end loop;

            if Present (Current) then
               Error_Msg (Flags,
                          """" &
                          Get_Name_String (Token_Name) &
                          """ is already a variable name", Token_Ptr);
            else
               Set_Next_String_Type
                 (String_Type, In_Tree,
                  To => First_String_Type_Of (Current_Project, In_Tree));
               Set_First_String_Type_Of
                 (Current_Project, In_Tree, To => String_Type);
            end if;
         end if;

         --  Scan past the name

         Scan (In_Tree);
      end if;

      Expect (Tok_Is, "IS");

      if Token = Tok_Is then
         Scan (In_Tree);
      end if;

      Expect (Tok_Left_Paren, "`(`");

      if Token = Tok_Left_Paren then
         Scan (In_Tree);
      end if;

      Parse_String_Type_List
        (In_Tree => In_Tree, First_String => First_String, Flags => Flags);
      Set_First_Literal_String (String_Type, In_Tree, To => First_String);

      Expect (Tok_Right_Paren, "`)`");

      if Token = Tok_Right_Paren then
         Scan (In_Tree);
      end if;

   end Parse_String_Type_Declaration;

   --------------------------------
   -- Parse_Variable_Declaration --
   --------------------------------

   procedure Parse_Variable_Declaration
     (In_Tree         : Project_Node_Tree_Ref;
      Variable        : out Project_Node_Id;
      Current_Project : Project_Node_Id;
      Current_Package : Project_Node_Id;
      Flags           : Processing_Flags)
   is
      Expression_Location      : Source_Ptr;
      String_Type_Name         : Name_Id := No_Name;
      Project_String_Type_Name : Name_Id := No_Name;
      Type_Location            : Source_Ptr := No_Location;
      Project_Location         : Source_Ptr := No_Location;
      Expression               : Project_Node_Id := Empty_Node;
      Variable_Name            : constant Name_Id := Token_Name;
      OK                       : Boolean := True;

   begin
      Variable :=
        Default_Project_Node
          (Of_Kind => N_Variable_Declaration, In_Tree => In_Tree);
      Set_Name_Of (Variable, In_Tree, To => Variable_Name);
      Set_Location_Of (Variable, In_Tree, To => Token_Ptr);

      --  Scan past the variable name

      Scan (In_Tree);

      if Token = Tok_Colon then

         --  Typed string variable declaration

         Scan (In_Tree);
         Set_Kind_Of (Variable, In_Tree, N_Typed_Variable_Declaration);
         Expect (Tok_Identifier, "identifier");

         OK := Token = Tok_Identifier;

         if OK then
            String_Type_Name := Token_Name;
            Type_Location := Token_Ptr;
            Scan (In_Tree);

            if Token = Tok_Dot then
               Project_String_Type_Name := String_Type_Name;
               Project_Location := Type_Location;

               --  Scan past the dot

               Scan (In_Tree);
               Expect (Tok_Identifier, "identifier");

               if Token = Tok_Identifier then
                  String_Type_Name := Token_Name;
                  Type_Location := Token_Ptr;
                  Scan (In_Tree);
               else
                  OK := False;
               end if;
            end if;

            if OK then
               declare
                  Proj    : Project_Node_Id := Current_Project;
                  Current : Project_Node_Id := Empty_Node;

               begin
                  if Project_String_Type_Name /= No_Name then
                     declare
                        The_Project_Name_And_Node : constant
                          Tree_Private_Part.Project_Name_And_Node :=
                          Tree_Private_Part.Projects_Htable.Get
                            (In_Tree.Projects_HT, Project_String_Type_Name);

                        use Tree_Private_Part;

                     begin
                        if The_Project_Name_And_Node =
                             Tree_Private_Part.No_Project_Name_And_Node
                        then
                           Error_Msg (Flags,
                                      "unknown project """ &
                                      Get_Name_String
                                         (Project_String_Type_Name) &
                                      """",
                                      Project_Location);
                           Current := Empty_Node;
                        else
                           Current :=
                             First_String_Type_Of
                               (The_Project_Name_And_Node.Node, In_Tree);
                           while
                             Present (Current)
                             and then
                               Name_Of (Current, In_Tree) /= String_Type_Name
                           loop
                              Current := Next_String_Type (Current, In_Tree);
                           end loop;
                        end if;
                     end;

                  else
                     --  Look for a string type with the correct name in this
                     --  project or in any of its ancestors.

                     loop
                        Current :=
                          First_String_Type_Of (Proj, In_Tree);
                        while
                          Present (Current)
                          and then
                            Name_Of (Current, In_Tree) /= String_Type_Name
                        loop
                           Current := Next_String_Type (Current, In_Tree);
                        end loop;

                        exit when Present (Current);

                        Proj := Parent_Project_Of (Proj, In_Tree);
                        exit when No (Proj);
                     end loop;
                  end if;

                  if No (Current) then
                     Error_Msg (Flags,
                                "unknown string type """ &
                                Get_Name_String (String_Type_Name) &
                                """",
                                Type_Location);
                     OK := False;

                  else
                     Set_String_Type_Of
                       (Variable, In_Tree, To => Current);
                  end if;
               end;
            end if;
         end if;
      end if;

      Expect (Tok_Colon_Equal, "`:=`");

      OK := OK and then Token = Tok_Colon_Equal;

      if Token = Tok_Colon_Equal then
         Scan (In_Tree);
      end if;

      --  Get the single string or string list value

      Expression_Location := Token_Ptr;

      Parse_Expression
        (In_Tree         => In_Tree,
         Expression      => Expression,
         Flags           => Flags,
         Current_Project => Current_Project,
         Current_Package => Current_Package,
         Optional_Index  => False);
      Set_Expression_Of (Variable, In_Tree, To => Expression);

      if Present (Expression) then
         --  A typed string must have a single string value, not a list

         if Kind_Of (Variable, In_Tree) = N_Typed_Variable_Declaration
           and then Expression_Kind_Of (Expression, In_Tree) = List
         then
            Error_Msg
              (Flags,
               "expression must be a single string", Expression_Location);
         end if;

         Set_Expression_Kind_Of
           (Variable, In_Tree,
            To => Expression_Kind_Of (Expression, In_Tree));
      end if;

      if OK then
         declare
            The_Variable : Project_Node_Id := Empty_Node;

         begin
            if Present (Current_Package) then
               The_Variable := First_Variable_Of (Current_Package, In_Tree);
            elsif Present (Current_Project) then
               The_Variable := First_Variable_Of (Current_Project, In_Tree);
            end if;

            while Present (The_Variable)
              and then Name_Of (The_Variable, In_Tree) /= Variable_Name
            loop
               The_Variable := Next_Variable (The_Variable, In_Tree);
            end loop;

            if No (The_Variable) then
               if Present (Current_Package) then
                  Set_Next_Variable
                    (Variable, In_Tree,
                     To => First_Variable_Of (Current_Package, In_Tree));
                  Set_First_Variable_Of
                    (Current_Package, In_Tree, To => Variable);

               elsif Present (Current_Project) then
                  Set_Next_Variable
                    (Variable, In_Tree,
                     To => First_Variable_Of (Current_Project, In_Tree));
                  Set_First_Variable_Of
                    (Current_Project, In_Tree, To => Variable);
               end if;

            else
               if Expression_Kind_Of (Variable, In_Tree) /= Undefined then
                  if Expression_Kind_Of (The_Variable, In_Tree) =
                                                            Undefined
                  then
                     Set_Expression_Kind_Of
                       (The_Variable, In_Tree,
                        To => Expression_Kind_Of (Variable, In_Tree));

                  else
                     if Expression_Kind_Of (The_Variable, In_Tree) /=
                       Expression_Kind_Of (Variable, In_Tree)
                     then
                        Error_Msg (Flags,
                                   "wrong expression kind for variable """ &
                                   Get_Name_String
                                     (Name_Of (The_Variable, In_Tree)) &
                                     """",
                                   Expression_Location);
                     end if;
                  end if;
               end if;
            end if;
         end;
      end if;
   end Parse_Variable_Declaration;

end Prj.Dect;
