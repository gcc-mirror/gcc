------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . D E C T                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2001-2003 Free Software Foundation, Inc          --
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

with Err_Vars; use Err_Vars;
with Namet;    use Namet;
with Prj.Err;  use Prj.Err;
with Prj.Strt; use Prj.Strt;
with Prj.Tree; use Prj.Tree;
with Scans;    use Scans;
with Snames;
with Types;    use Types;
with Prj.Attr; use Prj.Attr;

package body Prj.Dect is

   type Zone is (In_Project, In_Package, In_Case_Construction);
   --  Used to indicate if we are parsing a package (In_Package),
   --  a case construction (In_Case_Construction) or none of those two
   --  (In_Project).

   procedure Parse_Attribute_Declaration
     (Attribute         : out Project_Node_Id;
      First_Attribute   : Attribute_Node_Id;
      Current_Project   : Project_Node_Id;
      Current_Package   : Project_Node_Id);
   --  Parse an attribute declaration.

   procedure Parse_Case_Construction
     (Case_Construction : out Project_Node_Id;
      First_Attribute   : Attribute_Node_Id;
      Current_Project   : Project_Node_Id;
      Current_Package   : Project_Node_Id);
   --  Parse a case construction

   procedure Parse_Declarative_Items
     (Declarations      : out Project_Node_Id;
      In_Zone           : Zone;
      First_Attribute   : Attribute_Node_Id;
      Current_Project   : Project_Node_Id;
      Current_Package   : Project_Node_Id);
   --  Parse declarative items. Depending on In_Zone, some declarative
   --  items may be forbiden.

   procedure Parse_Package_Declaration
     (Package_Declaration : out Project_Node_Id;
      Current_Project     : Project_Node_Id);
   --  Parse a package declaration

   procedure Parse_String_Type_Declaration
     (String_Type     : out Project_Node_Id;
      Current_Project : Project_Node_Id);
   --  type <name> is ( <literal_string> { , <literal_string> } ) ;

   procedure Parse_Variable_Declaration
     (Variable        : out Project_Node_Id;
      Current_Project : Project_Node_Id;
      Current_Package : Project_Node_Id);
   --  Parse a variable assignment
   --  <variable_Name> := <expression>; OR
   --  <variable_Name> : <string_type_Name> := <string_expression>;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Declarations    : out Project_Node_Id;
      Current_Project : Project_Node_Id;
      Extends         : Project_Node_Id)
   is
      First_Declarative_Item : Project_Node_Id := Empty_Node;

   begin
      Declarations := Default_Project_Node (Of_Kind => N_Project_Declaration);
      Set_Location_Of (Declarations, To => Token_Ptr);
      Set_Extended_Project_Of (Declarations, To => Extends);
      Set_Project_Declaration_Of (Current_Project, Declarations);
      Parse_Declarative_Items
        (Declarations    => First_Declarative_Item,
         In_Zone         => In_Project,
         First_Attribute => Prj.Attr.Attribute_First,
         Current_Project => Current_Project,
         Current_Package => Empty_Node);
      Set_First_Declarative_Item_Of
        (Declarations, To => First_Declarative_Item);
   end Parse;

   ---------------------------------
   -- Parse_Attribute_Declaration --
   ---------------------------------

   procedure Parse_Attribute_Declaration
     (Attribute       : out Project_Node_Id;
      First_Attribute : Attribute_Node_Id;
      Current_Project : Project_Node_Id;
      Current_Package : Project_Node_Id)
   is
      Current_Attribute      : Attribute_Node_Id := First_Attribute;
      Full_Associative_Array : Boolean           := False;
      Attribute_Name         : Name_Id           := No_Name;

   begin
      Attribute := Default_Project_Node (Of_Kind => N_Attribute_Declaration);
      Set_Location_Of (Attribute, To => Token_Ptr);
      Set_Previous_Line_Node (Attribute);

      --  Scan past "for"

      Scan;

      --  Body may be an attribute name

      if Token = Tok_Body then
         Token := Tok_Identifier;
         Token_Name := Snames.Name_Body;
      end if;

      Expect (Tok_Identifier, "identifier");

      if Token = Tok_Identifier then
         Attribute_Name := Token_Name;
         Set_Name_Of (Attribute, To => Token_Name);
         Set_Location_Of (Attribute, To => Token_Ptr);

         --  Find the attribute

         while Current_Attribute /= Empty_Attribute
           and then
             Attributes.Table (Current_Attribute).Name /= Token_Name
         loop
            Current_Attribute := Attributes.Table (Current_Attribute).Next;
         end loop;

         --  If not a valid attribute name, issue an error, or a warning
         --  if inside a package that does not need to be checked.

         if Current_Attribute = Empty_Attribute then
            declare
               Message : constant String :=
                 "undefined attribute """ &
                 Get_Name_String (Name_Of (Attribute)) & '"';

               Warning : Boolean :=
                 Current_Package /= Empty_Node
                 and then Current_Packages_To_Check /= All_Packages;

            begin
               if Warning then

                  --  Check that we are not in a package to check

                  Get_Name_String (Name_Of (Current_Package));

                  for Index in Current_Packages_To_Check'Range loop
                     if Name_Buffer (1 .. Name_Len) =
                       Current_Packages_To_Check (Index).all
                     then
                        Warning := False;
                        exit;
                     end if;
                  end loop;
               end if;

               if Warning then
                  Error_Msg ('?' & Message, Token_Ptr);

               else
                  Error_Msg (Message, Token_Ptr);
               end if;
            end;

         --  Set, if appropriate the index case insensitivity flag

         elsif Attributes.Table (Current_Attribute).Kind_2 =
           Case_Insensitive_Associative_Array
         then
            Set_Case_Insensitive (Attribute, To => True);
         end if;

         Scan; --  past the attribute name
      end if;

      --  Change obsolete names of attributes to the new names

      case Name_Of (Attribute) is
         when Snames.Name_Specification =>
            Set_Name_Of (Attribute, To => Snames.Name_Spec);

         when Snames.Name_Specification_Suffix =>
            Set_Name_Of (Attribute, To => Snames.Name_Spec_Suffix);

         when Snames.Name_Implementation =>
            Set_Name_Of (Attribute, To => Snames.Name_Body);

         when Snames.Name_Implementation_Suffix =>
            Set_Name_Of (Attribute, To => Snames.Name_Body_Suffix);

         when others =>
            null;
      end case;

      --  Associative array attributes

      if Token = Tok_Left_Paren then

         --  If the attribute is not an associative array attribute, report
         --  an error.

         if Current_Attribute /= Empty_Attribute
           and then Attributes.Table (Current_Attribute).Kind_2 = Single
         then
            Error_Msg ("the attribute """ &
                       Get_Name_String
                          (Attributes.Table (Current_Attribute).Name) &
                       """ cannot be an associative array",
                       Location_Of (Attribute));
         end if;

         Scan; --  past the left parenthesis
         Expect (Tok_String_Literal, "literal string");

         if Token = Tok_String_Literal then
            Set_Associative_Array_Index_Of (Attribute, Token_Name);
            Scan; --  past the literal string index
         end if;

         Expect (Tok_Right_Paren, "`)`");

         if Token = Tok_Right_Paren then
            Scan; --  past the right parenthesis
         end if;

      else
         --  If it is an associative array attribute and there are no left
         --  parenthesis, then this is a full associative array declaration.
         --  Flag it as such for later processing of its value.

         if Current_Attribute /= Empty_Attribute
           and then
             Attributes.Table (Current_Attribute).Kind_2 /= Single
         then
            Full_Associative_Array := True;
         end if;
      end if;

      --  Set the expression kind of the attribute

      if Current_Attribute /= Empty_Attribute then
         Set_Expression_Kind_Of
           (Attribute, To => Attributes.Table (Current_Attribute).Kind_1);
      end if;

      Expect (Tok_Use, "USE");

      if Token = Tok_Use then
         Scan;

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
                                   (Current_Project, Token_Name);

                  if The_Project = Empty_Node then
                     Error_Msg ("unknown project", Location);
                     Scan; --  past the project name

                  else
                     Project_Name := Token_Name;
                     Scan; --  past the project name

                     --  If this is inside a package, a dot followed by the
                     --  name of the package must followed the project name.

                     if Current_Package /= Empty_Node then
                        Expect (Tok_Dot, "`.`");

                        if Token /= Tok_Dot then
                           The_Project := Empty_Node;

                        else
                           Scan; --  past the dot
                           Expect (Tok_Identifier, "identifier");

                           if Token /= Tok_Identifier then
                              The_Project := Empty_Node;

                           --  If it is not the same package name, issue error

                           elsif Token_Name /= Name_Of (Current_Package) then
                              The_Project := Empty_Node;
                              Error_Msg
                                ("not the same package as " &
                                 Get_Name_String (Name_Of (Current_Package)),
                                 Token_Ptr);

                           else
                              The_Package := First_Package_Of (The_Project);

                              --  Look for the package node

                              while The_Package /= Empty_Node
                                and then Name_Of (The_Package) /= Token_Name
                              loop
                                 The_Package :=
                                   Next_Package_In_Project (The_Package);
                              end loop;

                              --  If the package cannot be found in the
                              --  project, issue an error.

                              if The_Package = Empty_Node then
                                 The_Project := Empty_Node;
                                 Error_Msg_Name_2 := Project_Name;
                                 Error_Msg_Name_1 := Token_Name;
                                 Error_Msg
                                   ("package % not declared in project %",
                                   Token_Ptr);
                              end if;

                              Scan; --  past the package name
                           end if;
                        end if;
                     end if;
                  end if;
               end if;

               if The_Project /= Empty_Node then

                  --  Looking for '<same attribute name>

                  Expect (Tok_Apostrophe, "`''`");

                  if Token /= Tok_Apostrophe then
                     The_Project := Empty_Node;

                  else
                     Scan; --  past the apostrophe
                     Expect (Tok_Identifier, "identifier");

                     if Token /= Tok_Identifier then
                        The_Project := Empty_Node;

                     else
                        --  If it is not the same attribute name, issue error

                        if Token_Name /= Attribute_Name then
                           The_Project := Empty_Node;
                           Error_Msg_Name_1 := Attribute_Name;
                           Error_Msg ("invalid name, should be %", Token_Ptr);
                        end if;

                        Scan; --  past the attribute name
                     end if;
                  end if;
               end if;

               if The_Project = Empty_Node then

                  --  If there were any problem, set the attribute id to null,
                  --  so that the node will not be recorded.

                  Current_Attribute := Empty_Attribute;

               else
                  --  Set the appropriate field in the node.
                  --  Note that the index and the expression are nil. This
                  --  characterizes full associative array attribute
                  --  declarations.

                  Set_Associative_Project_Of (Attribute, The_Project);
                  Set_Associative_Package_Of (Attribute, The_Package);
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
                 (Expression      => Expression,
                  Current_Project => Current_Project,
                  Current_Package => Current_Package);
               Set_Expression_Of (Attribute, To => Expression);

               --  If the expression is legal, but not of the right kind
               --  for the attribute, issue an error.

               if Current_Attribute /= Empty_Attribute
                 and then Expression /= Empty_Node
                 and then Attributes.Table (Current_Attribute).Kind_1 /=
                 Expression_Kind_Of (Expression)
               then
                  Error_Msg
                    ("wrong expression kind for attribute """ &
                     Get_Name_String
                       (Attributes.Table (Current_Attribute).Name) &
                     """",
                     Expression_Location);
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
     (Case_Construction : out Project_Node_Id;
      First_Attribute   : Attribute_Node_Id;
      Current_Project   : Project_Node_Id;
      Current_Package   : Project_Node_Id)
   is
      Current_Item    : Project_Node_Id := Empty_Node;
      Next_Item       : Project_Node_Id := Empty_Node;
      First_Case_Item : Boolean := True;

      Variable_Location : Source_Ptr := No_Location;

      String_Type : Project_Node_Id := Empty_Node;

      Case_Variable : Project_Node_Id := Empty_Node;

      First_Declarative_Item : Project_Node_Id := Empty_Node;

      First_Choice : Project_Node_Id := Empty_Node;

   begin
      Case_Construction  :=
        Default_Project_Node (Of_Kind => N_Case_Construction);
      Set_Location_Of (Case_Construction, To => Token_Ptr);

      --  Scan past "case"

      Scan;

      --  Get the switch variable

      Expect (Tok_Identifier, "identifier");

      if Token = Tok_Identifier then
         Variable_Location := Token_Ptr;
         Parse_Variable_Reference
           (Variable        => Case_Variable,
            Current_Project => Current_Project,
            Current_Package => Current_Package);
         Set_Case_Variable_Reference_Of
           (Case_Construction, To => Case_Variable);

      else
         if Token /= Tok_Is then
            Scan;
         end if;
      end if;

      if Case_Variable /= Empty_Node then
         String_Type := String_Type_Of (Case_Variable);

         if String_Type = Empty_Node then
            Error_Msg ("variable """ &
                       Get_Name_String (Name_Of (Case_Variable)) &
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

         Scan;
      end if;

      Start_New_Case_Construction (String_Type);

      When_Loop :

      while Token = Tok_When loop

         if First_Case_Item then
            Current_Item := Default_Project_Node (Of_Kind => N_Case_Item);
            Set_First_Case_Item_Of (Case_Construction, To => Current_Item);
            First_Case_Item := False;

         else
            Next_Item := Default_Project_Node (Of_Kind => N_Case_Item);
            Set_Next_Case_Item (Current_Item, To => Next_Item);
            Current_Item := Next_Item;
         end if;

         Set_Location_Of (Current_Item, To => Token_Ptr);

         --  Scan past "when"

         Scan;

         if Token = Tok_Others then

            --  Scan past "others"

            Scan;

            Expect (Tok_Arrow, "`=>`");
            Set_End_Of_Line (Current_Item);
            Set_Previous_Line_Node (Current_Item);

            --  Empty_Node in Field1 of a Case_Item indicates
            --  the "when others =>" branch.

            Set_First_Choice_Of (Current_Item, To => Empty_Node);

            Parse_Declarative_Items
              (Declarations    => First_Declarative_Item,
               In_Zone         => In_Case_Construction,
               First_Attribute => First_Attribute,
               Current_Project => Current_Project,
               Current_Package => Current_Package);

            --  "when others =>" must be the last branch, so save the
            --  Case_Item and exit

            Set_First_Declarative_Item_Of
              (Current_Item, To => First_Declarative_Item);
            exit When_Loop;

         else
            Parse_Choice_List (First_Choice => First_Choice);
            Set_First_Choice_Of (Current_Item, To => First_Choice);

            Expect (Tok_Arrow, "`=>`");
            Set_End_Of_Line (Current_Item);
            Set_Previous_Line_Node (Current_Item);

            Parse_Declarative_Items
              (Declarations    => First_Declarative_Item,
               In_Zone         => In_Case_Construction,
               First_Attribute => First_Attribute,
               Current_Project => Current_Project,
               Current_Package => Current_Package);

            Set_First_Declarative_Item_Of
              (Current_Item, To => First_Declarative_Item);

         end if;
      end loop When_Loop;

      End_Case_Construction;

      Expect (Tok_End, "`END CASE`");
      Remove_Next_End_Node;

      if Token = Tok_End then

         --  Scan past "end"

         Scan;

         Expect (Tok_Case, "CASE");

      end if;

      --  Scan past "case"

      Scan;

      Expect (Tok_Semicolon, "`;`");
      Set_Previous_End_Node (Case_Construction);

   end Parse_Case_Construction;

   -----------------------------
   -- Parse_Declarative_Items --
   -----------------------------

   procedure Parse_Declarative_Items
     (Declarations    : out Project_Node_Id;
      In_Zone         : Zone;
      First_Attribute : Attribute_Node_Id;
      Current_Project : Project_Node_Id;
      Current_Package : Project_Node_Id)
   is
      Current_Declarative_Item : Project_Node_Id := Empty_Node;
      Next_Declarative_Item    : Project_Node_Id := Empty_Node;
      Current_Declaration      : Project_Node_Id := Empty_Node;
      Item_Location            : Source_Ptr      := No_Location;

   begin
      Declarations := Empty_Node;

      loop
         --  We are always positioned at the token that precedes
         --  the first token of the declarative element.
         --  Scan past it

         Scan;

         Item_Location := Token_Ptr;

         case Token is
            when Tok_Identifier =>

               if In_Zone = In_Case_Construction then
                  Error_Msg ("a variable cannot be declared here",
                             Token_Ptr);
               end if;

               Parse_Variable_Declaration
                 (Current_Declaration,
                  Current_Project => Current_Project,
                  Current_Package => Current_Package);

               Set_End_Of_Line (Current_Declaration);
               Set_Previous_Line_Node (Current_Declaration);

            when Tok_For =>

               Parse_Attribute_Declaration
                 (Attribute       => Current_Declaration,
                  First_Attribute => First_Attribute,
                  Current_Project => Current_Project,
                  Current_Package => Current_Package);

               Set_End_Of_Line (Current_Declaration);
               Set_Previous_Line_Node (Current_Declaration);

            when Tok_Package =>

               --  Package declaration

               if In_Zone /= In_Project then
                  Error_Msg ("a package cannot be declared here", Token_Ptr);
               end if;

               Parse_Package_Declaration
                 (Package_Declaration => Current_Declaration,
                  Current_Project     => Current_Project);

               Set_Previous_End_Node (Current_Declaration);

            when Tok_Type =>

               --  Type String Declaration

               if In_Zone /= In_Project then
                  Error_Msg ("a string type cannot be declared here",
                             Token_Ptr);
               end if;

               Parse_String_Type_Declaration
                 (String_Type     => Current_Declaration,
                  Current_Project => Current_Project);

               Set_End_Of_Line (Current_Declaration);
               Set_Previous_Line_Node (Current_Declaration);

            when Tok_Case =>

               --  Case construction

               Parse_Case_Construction
                 (Case_Construction => Current_Declaration,
                  First_Attribute   => First_Attribute,
                  Current_Project   => Current_Project,
                  Current_Package   => Current_Package);

               Set_Previous_End_Node (Current_Declaration);

            when others =>
               exit;

               --  We are leaving Parse_Declarative_Items positionned
               --  at the first token after the list of declarative items.
               --  It could be "end" (for a project, a package declaration or
               --  a case construction) or "when" (for a case construction)

         end case;

         Expect (Tok_Semicolon, "`;` after declarative items");

         --  Insert an N_Declarative_Item in the tree, but only if
         --  Current_Declaration is not an empty node.

         if Current_Declaration /= Empty_Node then
            if Current_Declarative_Item = Empty_Node then
               Current_Declarative_Item :=
                 Default_Project_Node (Of_Kind => N_Declarative_Item);
               Declarations  := Current_Declarative_Item;

            else
               Next_Declarative_Item :=
                 Default_Project_Node (Of_Kind => N_Declarative_Item);
               Set_Next_Declarative_Item
                 (Current_Declarative_Item, To => Next_Declarative_Item);
               Current_Declarative_Item := Next_Declarative_Item;
            end if;

            Set_Current_Item_Node
              (Current_Declarative_Item, To => Current_Declaration);
            Set_Location_Of (Current_Declarative_Item, To => Item_Location);
         end if;

      end loop;

   end Parse_Declarative_Items;

   -------------------------------
   -- Parse_Package_Declaration --
   -------------------------------

   procedure Parse_Package_Declaration
     (Package_Declaration : out Project_Node_Id;
      Current_Project     : Project_Node_Id)
   is
      First_Attribute        : Attribute_Node_Id := Empty_Attribute;
      Current_Package        : Package_Node_Id   := Empty_Package;
      First_Declarative_Item : Project_Node_Id   := Empty_Node;

   begin
      Package_Declaration :=
        Default_Project_Node (Of_Kind => N_Package_Declaration);
      Set_Location_Of (Package_Declaration, To => Token_Ptr);

      --  Scan past "package"

      Scan;

      Expect (Tok_Identifier, "identifier");

      if Token = Tok_Identifier then

         Set_Name_Of (Package_Declaration, To => Token_Name);

         for Index in Package_Attributes.First .. Package_Attributes.Last loop
            if Token_Name = Package_Attributes.Table (Index).Name then
               First_Attribute :=
                 Package_Attributes.Table (Index).First_Attribute;
               Current_Package := Index;
               exit;
            end if;
         end loop;

         if Current_Package  = Empty_Package then
            Error_Msg ("?""" &
                       Get_Name_String (Name_Of (Package_Declaration)) &
                       """ is not an allowed package name",
                       Token_Ptr);

            --  Set the package declaration to "ignored" so that it is not
            --  processed by Prj.Proc.Process.

            Set_Expression_Kind_Of (Package_Declaration, Ignored);

         else
            Set_Package_Id_Of (Package_Declaration, To => Current_Package);

            declare
               Current : Project_Node_Id := First_Package_Of (Current_Project);

            begin
               while Current /= Empty_Node
                 and then Name_Of (Current) /= Token_Name
               loop
                  Current := Next_Package_In_Project (Current);
               end loop;

               if Current /= Empty_Node then
                  Error_Msg
                    ("package """ &
                     Get_Name_String (Name_Of (Package_Declaration)) &
                     """ is declared twice in the same project",
                     Token_Ptr);

               else
                  --  Add the package to the project list

                  Set_Next_Package_In_Project
                    (Package_Declaration,
                     To => First_Package_Of (Current_Project));
                  Set_First_Package_Of
                    (Current_Project, To => Package_Declaration);
               end if;
            end;
         end if;

         --  Scan past the package name

         Scan;
      end if;

      if Token = Tok_Renames then

         --  Scan past "renames"

         Scan;

         Expect (Tok_Identifier, "identifier");

         if Token = Tok_Identifier then
            declare
               Project_Name : constant Name_Id := Token_Name;
               Clause       : Project_Node_Id :=
                                First_With_Clause_Of (Current_Project);
               The_Project  : Project_Node_Id := Empty_Node;
               Extended     : constant Project_Node_Id :=
                                Extended_Project_Of
                                  (Project_Declaration_Of (Current_Project));
            begin
               while Clause /= Empty_Node loop
                  --  Only non limited imported projects may be used
                  --  in a renames declaration.

                  The_Project := Non_Limited_Project_Node_Of (Clause);
                  exit when The_Project /= Empty_Node
                    and then Name_Of (The_Project) = Project_Name;
                  Clause := Next_With_Clause_Of (Clause);
               end loop;

               if Clause = Empty_Node then
                  --  As we have not found the project in the imports, we check
                  --  if it's the name of an eventual extended project.

                  if Extended /= Empty_Node
                    and then Name_Of (Extended) = Project_Name then
                     Set_Project_Of_Renamed_Package_Of
                       (Package_Declaration, To => Extended);
                  else
                     Error_Msg_Name_1 := Project_Name;
                     Error_Msg
                       ("% is not an imported or extended project", Token_Ptr);
                  end if;
               else
                  Set_Project_Of_Renamed_Package_Of
                    (Package_Declaration, To => The_Project);
               end if;
            end;

            Scan;
            Expect (Tok_Dot, "`.`");

            if Token = Tok_Dot then
               Scan;
               Expect (Tok_Identifier, "identifier");

               if Token = Tok_Identifier then
                  if Name_Of (Package_Declaration) /= Token_Name then
                     Error_Msg ("not the same package name", Token_Ptr);
                  elsif
                    Project_Of_Renamed_Package_Of (Package_Declaration)
                                                              /= Empty_Node
                  then
                     declare
                        Current : Project_Node_Id :=
                                    First_Package_Of
                                      (Project_Of_Renamed_Package_Of
                                         (Package_Declaration));

                     begin
                        while Current /= Empty_Node
                          and then Name_Of (Current) /= Token_Name
                        loop
                           Current := Next_Package_In_Project (Current);
                        end loop;

                        if Current = Empty_Node then
                           Error_Msg
                             ("""" &
                              Get_Name_String (Token_Name) &
                              """ is not a package declared by the project",
                              Token_Ptr);
                        end if;
                     end;
                  end if;

                  Scan;
               end if;
            end if;
         end if;

         Expect (Tok_Semicolon, "`;`");
         Set_End_Of_Line (Package_Declaration);
         Set_Previous_Line_Node (Package_Declaration);

      elsif Token = Tok_Is then
         Set_End_Of_Line (Package_Declaration);
         Set_Previous_Line_Node (Package_Declaration);
         Set_Next_End_Node (Package_Declaration);

         Parse_Declarative_Items
           (Declarations    => First_Declarative_Item,
            In_Zone         => In_Package,
            First_Attribute => First_Attribute,
            Current_Project => Current_Project,
            Current_Package => Package_Declaration);

         Set_First_Declarative_Item_Of
           (Package_Declaration, To => First_Declarative_Item);

         Expect (Tok_End, "END");

         if Token = Tok_End then

            --  Scan past "end"

            Scan;
         end if;

         --  We should have the name of the package after "end"

         Expect (Tok_Identifier, "identifier");

         if Token = Tok_Identifier
           and then Name_Of (Package_Declaration) /= No_Name
           and then Token_Name /= Name_Of (Package_Declaration)
         then
            Error_Msg_Name_1 := Name_Of (Package_Declaration);
            Error_Msg ("expected {", Token_Ptr);
         end if;

         if Token /= Tok_Semicolon then

            --  Scan past the package name

            Scan;
         end if;

         Expect (Tok_Semicolon, "`;`");
         Remove_Next_End_Node;

      else
         Error_Msg ("expected IS or RENAMES", Token_Ptr);
      end if;

   end Parse_Package_Declaration;

   -----------------------------------
   -- Parse_String_Type_Declaration --
   -----------------------------------

   procedure Parse_String_Type_Declaration
     (String_Type     : out Project_Node_Id;
      Current_Project : Project_Node_Id)
   is
      Current      : Project_Node_Id := Empty_Node;
      First_String : Project_Node_Id := Empty_Node;

   begin
      String_Type :=
        Default_Project_Node (Of_Kind => N_String_Type_Declaration);

      Set_Location_Of (String_Type, To => Token_Ptr);

      --  Scan past "type"

      Scan;

      Expect (Tok_Identifier, "identifier");

      if Token = Tok_Identifier then
         Set_Name_Of (String_Type, To => Token_Name);

         Current := First_String_Type_Of (Current_Project);
         while Current /= Empty_Node
           and then
           Name_Of (Current) /= Token_Name
         loop
            Current := Next_String_Type (Current);
         end loop;

         if Current /= Empty_Node then
            Error_Msg ("duplicate string type name """ &
                       Get_Name_String (Token_Name) &
                       """",
                       Token_Ptr);
         else
            Current := First_Variable_Of (Current_Project);
            while Current /= Empty_Node
              and then Name_Of (Current) /= Token_Name
            loop
               Current := Next_Variable (Current);
            end loop;

            if Current /= Empty_Node then
               Error_Msg ("""" &
                          Get_Name_String (Token_Name) &
                          """ is already a variable name", Token_Ptr);
            else
               Set_Next_String_Type
                 (String_Type, To => First_String_Type_Of (Current_Project));
               Set_First_String_Type_Of (Current_Project, To => String_Type);
            end if;
         end if;

         --  Scan past the name

         Scan;
      end if;

      Expect (Tok_Is, "IS");

      if Token = Tok_Is then
         Scan;
      end if;

      Expect (Tok_Left_Paren, "`(`");

      if Token = Tok_Left_Paren then
         Scan;
      end if;

      Parse_String_Type_List (First_String => First_String);
      Set_First_Literal_String (String_Type, To => First_String);

      Expect (Tok_Right_Paren, "`)`");

      if Token = Tok_Right_Paren then
         Scan;
      end if;

   end Parse_String_Type_Declaration;

   --------------------------------
   -- Parse_Variable_Declaration --
   --------------------------------

   procedure Parse_Variable_Declaration
     (Variable        : out Project_Node_Id;
      Current_Project : Project_Node_Id;
      Current_Package : Project_Node_Id)
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
        Default_Project_Node (Of_Kind => N_Variable_Declaration);
      Set_Name_Of (Variable, To => Variable_Name);
      Set_Location_Of (Variable, To => Token_Ptr);

      --  Scan past the variable name

      Scan;

      if Token = Tok_Colon then

         --  Typed string variable declaration

         Scan;
         Set_Kind_Of (Variable, N_Typed_Variable_Declaration);
         Expect (Tok_Identifier, "identifier");

         OK := Token = Tok_Identifier;

         if OK then
            String_Type_Name := Token_Name;
            Type_Location := Token_Ptr;
            Scan;

            if Token = Tok_Dot then
               Project_String_Type_Name := String_Type_Name;
               Project_Location := Type_Location;

               --  Scan past the dot

               Scan;
               Expect (Tok_Identifier, "identifier");

               if Token = Tok_Identifier then
                  String_Type_Name := Token_Name;
                  Type_Location := Token_Ptr;
                  Scan;
               else
                  OK := False;
               end if;
            end if;

            if OK then
               declare
                  Current : Project_Node_Id :=
                              First_String_Type_Of (Current_Project);

               begin
                  if Project_String_Type_Name /= No_Name then
                     declare
                        The_Project_Name_And_Node : constant
                          Tree_Private_Part.Project_Name_And_Node :=
                          Tree_Private_Part.Projects_Htable.Get
                                                    (Project_String_Type_Name);

                        use Tree_Private_Part;

                     begin
                        if The_Project_Name_And_Node =
                          Tree_Private_Part.No_Project_Name_And_Node
                        then
                           Error_Msg ("unknown project """ &
                                      Get_Name_String
                                         (Project_String_Type_Name) &
                                      """",
                                      Project_Location);
                           Current := Empty_Node;
                        else
                           Current :=
                             First_String_Type_Of
                                         (The_Project_Name_And_Node.Node);
                        end if;
                     end;
                  end if;

                  while Current /= Empty_Node
                    and then Name_Of (Current) /= String_Type_Name
                  loop
                     Current := Next_String_Type (Current);
                  end loop;

                  if Current = Empty_Node then
                     Error_Msg ("unknown string type """ &
                                Get_Name_String (String_Type_Name) &
                                """",
                                Type_Location);
                     OK := False;
                  else
                     Set_String_Type_Of
                       (Variable, To => Current);
                  end if;
               end;
            end if;
         end if;
      end if;

      Expect (Tok_Colon_Equal, "`:=`");

      OK := OK and (Token = Tok_Colon_Equal);

      if Token = Tok_Colon_Equal then
         Scan;
      end if;

      --  Get the single string or string list value

      Expression_Location := Token_Ptr;

      Parse_Expression
        (Expression      => Expression,
         Current_Project => Current_Project,
         Current_Package => Current_Package);
      Set_Expression_Of (Variable, To => Expression);

      if Expression /= Empty_Node then
         --  A typed string must have a single string value, not a list

         if Kind_Of (Variable) = N_Typed_Variable_Declaration
           and then Expression_Kind_Of (Expression) = List
         then
            Error_Msg
              ("expression must be a single string", Expression_Location);
         end if;

         Set_Expression_Kind_Of
           (Variable, To => Expression_Kind_Of (Expression));
      end if;

      if OK then
         declare
            The_Variable : Project_Node_Id := Empty_Node;

         begin
            if Current_Package /= Empty_Node then
               The_Variable :=  First_Variable_Of (Current_Package);
            elsif Current_Project /= Empty_Node then
               The_Variable :=  First_Variable_Of (Current_Project);
            end if;

            while The_Variable /= Empty_Node
              and then Name_Of (The_Variable) /= Variable_Name
            loop
               The_Variable := Next_Variable (The_Variable);
            end loop;

            if The_Variable = Empty_Node then
               if Current_Package /= Empty_Node then
                  Set_Next_Variable
                    (Variable, To => First_Variable_Of (Current_Package));
                  Set_First_Variable_Of (Current_Package, To => Variable);

               elsif Current_Project /= Empty_Node then
                  Set_Next_Variable
                    (Variable, To => First_Variable_Of (Current_Project));
                  Set_First_Variable_Of (Current_Project, To => Variable);
               end if;

            else
               if Expression_Kind_Of (Variable) /= Undefined then
                  if Expression_Kind_Of (The_Variable) = Undefined then
                     Set_Expression_Kind_Of
                       (The_Variable, To => Expression_Kind_Of (Variable));

                  else
                     if Expression_Kind_Of (The_Variable) /=
                       Expression_Kind_Of (Variable)
                     then
                        Error_Msg ("wrong expression kind for variable """ &
                                     Get_Name_String (Name_Of (The_Variable)) &
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
