------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . D E C T                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.3 $
--                                                                          --
--             Copyright (C) 2001 Free Software Foundation, Inc.            --
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

with Errout;     use Errout;
with Prj.Strt;
with Prj.Tree;   use Prj.Tree;
with Scans;      use Scans;
with Sinfo;      use Sinfo;
with Types;      use Types;
with Prj.Attr;   use Prj.Attr;

package body Prj.Dect is

   type Zone is (In_Project, In_Package, In_Case_Construction);

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
     (String_Type       : out Project_Node_Id;
      Current_Project   : Project_Node_Id;
      First_Attribute   : Attribute_Node_Id);
   --  type <name> is ( <literal_string> { , <literal_string> } ) ;

   procedure Parse_Variable_Declaration
     (Variable          : out Project_Node_Id;
      First_Attribute   : Attribute_Node_Id;
      Current_Project   : Project_Node_Id;
      Current_Package   : Project_Node_Id);
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
      Set_Modified_Project_Of (Declarations, To => Extends);
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
      Current_Attribute : Attribute_Node_Id := First_Attribute;

   begin
      Attribute := Default_Project_Node (Of_Kind => N_Attribute_Declaration);
      Set_Location_Of (Attribute, To => Token_Ptr);

      --  Scan past "for"

      Scan;

      Expect (Tok_Identifier, "identifier");

      if Token = Tok_Identifier then
         Set_Name_Of (Attribute, To => Token_Name);
         Set_Location_Of (Attribute, To => Token_Ptr);

         if Attributes.Table (Current_Attribute).Kind_2 =
                            Case_Insensitive_Associative_Array
         then
            Set_Case_Insensitive (Attribute, To => True);
         end if;

         while Current_Attribute /= Empty_Attribute
           and then
             Attributes.Table (Current_Attribute).Name /= Token_Name
         loop
            Current_Attribute := Attributes.Table (Current_Attribute).Next;
         end loop;

         if Current_Attribute = Empty_Attribute then
            Error_Msg ("undefined attribute", Token_Ptr);
         end if;

         Scan;
      end if;

      if Token = Tok_Left_Paren then
         if Current_Attribute /= Empty_Attribute
           and then Attributes.Table (Current_Attribute).Kind_2 = Single
         then
            Error_Msg ("this attribute cannot be an associative array",
                       Location_Of (Attribute));
         end if;

         Scan;
         Expect (Tok_String_Literal, "literal string");

         if Token = Tok_String_Literal then
            Set_Associative_Array_Index_Of (Attribute, Strval (Token_Node));
            Scan;
         end if;

         Expect (Tok_Right_Paren, ")");

         if Token = Tok_Right_Paren then
            Scan;
         end if;

      else
         if Current_Attribute /= Empty_Attribute
           and then
             Attributes.Table (Current_Attribute).Kind_2 /= Single
         then
            Error_Msg ("this attribute need to be an associative array",
                       Location_Of (Attribute));
         end if;
      end if;

      if Current_Attribute /= Empty_Attribute then
         Set_Expression_Kind_Of
           (Attribute, To => Attributes.Table (Current_Attribute).Kind_1);
      end if;

      Expect (Tok_Use, "use");

      if Token = Tok_Use then
         Scan;

         declare
            Expression_Location : constant Source_Ptr := Token_Ptr;
            Expression          : Project_Node_Id     := Empty_Node;

         begin
            Prj.Strt.Parse_Expression
              (Expression      => Expression,
               Current_Project => Current_Project,
               Current_Package => Current_Package);
            Set_Expression_Of (Attribute, To => Expression);

            if Current_Attribute /= Empty_Attribute
              and then Expression /= Empty_Node
              and then Attributes.Table (Current_Attribute).Kind_1 /=
                                          Expression_Kind_Of (Expression)
            then
               Error_Msg
                 ("wrong expression kind for the attribute",
                  Expression_Location);
            end if;
         end;
      end if;

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
      Current_Item      : Project_Node_Id := Empty_Node;
      Next_Item         : Project_Node_Id := Empty_Node;
      First_Case_Item   : Boolean := True;

      Variable_Location : Source_Ptr := No_Location;

      String_Type       : Project_Node_Id := Empty_Node;

      Case_Variable     : Project_Node_Id := Empty_Node;

      First_Declarative_Item : Project_Node_Id := Empty_Node;

      First_Choice      : Project_Node_Id := Empty_Node;

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
         Prj.Strt.Parse_Variable_Reference
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
            Error_Msg ("this variable is not typed", Variable_Location);
         end if;
      end if;

      Expect (Tok_Is, "is");

      if Token = Tok_Is then

         --  Scan past "is"

         Scan;
      end if;

      Prj.Strt.Start_New_Case_Construction (String_Type);

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

            Expect (Tok_Arrow, "=>");

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
            Prj.Strt.Parse_Choice_List (First_Choice => First_Choice);
            Set_First_Choice_Of (Current_Item, To => First_Choice);

            Expect (Tok_Arrow, "=>");

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

      Prj.Strt.End_Case_Construction;

      Expect (Tok_End, "end case");

      if Token = Tok_End then

         --  Scan past "end"

         Scan;

         Expect (Tok_Case, "case");

      end if;

      --  Scan past "case"

      Scan;

      Expect (Tok_Semicolon, ";");

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
                  First_Attribute => First_Attribute,
                  Current_Project => Current_Project,
                  Current_Package => Current_Package);

            when Tok_For =>

               Parse_Attribute_Declaration
                 (Attribute       => Current_Declaration,
                  First_Attribute => First_Attribute,
                  Current_Project => Current_Project,
                  Current_Package => Current_Package);

            when Tok_Package =>

               --  Package declaration

               if In_Zone /= In_Project then
                  Error_Msg ("a package cannot be declared here", Token_Ptr);
               end if;

               Parse_Package_Declaration
                 (Package_Declaration => Current_Declaration,
                  Current_Project     => Current_Project);

            when Tok_Type =>

               --  Type String Declaration

               if In_Zone /= In_Project then
                  Error_Msg ("a string type cannot be declared here",
                             Token_Ptr);
               end if;

               Parse_String_Type_Declaration
                 (String_Type     => Current_Declaration,
                  Current_Project => Current_Project,
                  First_Attribute => First_Attribute);

            when Tok_Case =>

               --  Case construction

               Parse_Case_Construction
                 (Case_Construction => Current_Declaration,
                  First_Attribute   => First_Attribute,
                  Current_Project   => Current_Project,
                  Current_Package   => Current_Package);

            when others =>
               exit;

               --  We are leaving Parse_Declarative_Items positionned
               --  at the first token after the list of declarative items.
               --  It could be "end" (for a project, a package declaration or
               --  a case construction) or "when" (for a case construction)

         end case;

         Expect (Tok_Semicolon, "; after declarative items");

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
            Error_Msg ("not an allowed package name", Token_Ptr);

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
                    ("package declared twice in the same project", Token_Ptr);

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
               Project_Name : Name_Id := Token_Name;
               Clause       : Project_Node_Id :=
                                First_With_Clause_Of (Current_Project);
               The_Project  : Project_Node_Id := Empty_Node;

            begin
               while Clause /= Empty_Node loop
                  The_Project := Project_Node_Of (Clause);
                  exit when Name_Of (The_Project) = Project_Name;
                  Clause := Next_With_Clause_Of (Clause);
               end loop;

               if Clause = Empty_Node then
                  Error_Msg ("not an imported project", Token_Ptr);
               else
                  Set_Project_Of_Renamed_Package_Of
                    (Package_Declaration, To => The_Project);
               end if;
            end;

            Scan;
            Expect (Tok_Dot, ".");

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
                             ("not a package declared by the project",
                              Token_Ptr);
                        end if;
                     end;
                  end if;

                  Scan;
               end if;
            end if;
         end if;

         Expect (Tok_Semicolon, ";");

      elsif Token = Tok_Is then

         Parse_Declarative_Items
           (Declarations    => First_Declarative_Item,
            In_Zone         => In_Package,
            First_Attribute => First_Attribute,
            Current_Project => Current_Project,
            Current_Package => Package_Declaration);

         Set_First_Declarative_Item_Of
           (Package_Declaration, To => First_Declarative_Item);

         Expect (Tok_End, "end");

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

         Expect (Tok_Semicolon, ";");

      else
         Error_Msg ("expected ""is"" or ""renames""", Token_Ptr);
      end if;

   end Parse_Package_Declaration;

   -----------------------------------
   -- Parse_String_Type_Declaration --
   -----------------------------------

   procedure Parse_String_Type_Declaration
     (String_Type     : out Project_Node_Id;
      Current_Project : Project_Node_Id;
      First_Attribute : Attribute_Node_Id)
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
            Error_Msg ("duplicate string type name", Token_Ptr);
         else
            Current := First_Variable_Of (Current_Project);
            while Current /= Empty_Node
              and then Name_Of (Current) /= Token_Name
            loop
               Current := Next_Variable (Current);
            end loop;

            if Current /= Empty_Node then
               Error_Msg ("already a variable name", Token_Ptr);
            else
               Set_Next_String_Type
                 (String_Type, To => First_String_Type_Of (Current_Project));
               Set_First_String_Type_Of (Current_Project, To => String_Type);
            end if;
         end if;

         --  Scan past the name

         Scan;
      end if;

      Expect (Tok_Is, "is");

      if Token = Tok_Is then
         Scan;
      end if;

      Expect (Tok_Left_Paren, "(");

      if Token = Tok_Left_Paren then
         Scan;
      end if;

      Prj.Strt.Parse_String_Type_List (First_String => First_String);
      Set_First_Literal_String (String_Type, To => First_String);

      Expect (Tok_Right_Paren, ")");

      if Token = Tok_Right_Paren then
         Scan;
      end if;

   end Parse_String_Type_Declaration;

   --------------------------------
   -- Parse_Variable_Declaration --
   --------------------------------

   procedure Parse_Variable_Declaration
     (Variable        : out Project_Node_Id;
      First_Attribute : Attribute_Node_Id;
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

         if Token = Tok_Identifier then
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
                  String_Type_Name := No_Name;
               end if;
            end if;

            if String_Type_Name /= No_Name then
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
                           Error_Msg ("unknown project", Project_Location);
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
                     Error_Msg ("unknown string type", Type_Location);
                  else
                     Set_String_Type_Of
                       (Variable, To => Current);
                  end if;
               end;
            end if;
         end if;
      end if;

      Expect (Tok_Colon_Equal, ":=");

      if Token = Tok_Colon_Equal then
         Scan;
      end if;

      --  Get the single string or string list value

      Expression_Location := Token_Ptr;

      Prj.Strt.Parse_Expression
        (Expression      => Expression,
         Current_Project => Current_Project,
         Current_Package => Current_Package);
      Set_Expression_Of (Variable, To => Expression);

      if Expression /= Empty_Node then
         Set_Expression_Kind_Of
           (Variable, To => Expression_Kind_Of (Expression));
      end if;

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
                     Error_Msg ("wrong expression kind for the variable",
                                Expression_Location);
                  end if;
               end if;
            end if;
         end if;
      end;

   end Parse_Variable_Declaration;

end Prj.Dect;
