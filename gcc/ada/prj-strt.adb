------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P R J . S T R T                              --
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

with Errout;    use Errout;
with Namet;     use Namet;
with Prj.Attr;  use Prj.Attr;
with Prj.Tree;  use Prj.Tree;
with Scans;     use Scans;
with Sinfo;     use Sinfo;
with Stringt;   use Stringt;
with Table;
with Types;     use Types;

package body Prj.Strt is

   type Name_Location is record
      Name     : Name_Id := No_Name;
      Location : Source_Ptr := No_Location;
   end record;
   --  Store the identifier and the location of a simple name

   type Name_Range is range 0 .. 3;
   subtype Name_Index is Name_Range range 1 .. Name_Range'Last;
   --  A Name may contain up to 3 simple names

   type Names is array (Name_Index) of Name_Location;
   --  Used to store 1 to 3 simple_names. 2 simple names are for
   --  <project>.<package>, <project>.<variable> or <package>.<variable>.
   --  3 simple names are for <project>.<package>.<variable>.

   type Choice_String is record
      The_String : String_Id;
      Already_Used : Boolean := False;
   end record;
   --  The string of a case label, and an indication that it has already
   --  been used (to avoid duplicate case labels).

   Choices_Initial   : constant := 10;
   Choices_Increment : constant := 10;

   Choice_Node_Low_Bound  : constant := 0;
   Choice_Node_High_Bound : constant := 099_999_999; --  In practice, infinite

   type Choice_Node_Id is
     range Choice_Node_Low_Bound .. Choice_Node_High_Bound;

   First_Choice_Node_Id : constant Choice_Node_Id :=
     Choice_Node_Low_Bound;

   package Choices is
      new Table.Table (Table_Component_Type => Choice_String,
                       Table_Index_Type     => Choice_Node_Id,
                       Table_Low_Bound      => First_Choice_Node_Id,
                       Table_Initial        => Choices_Initial,
                       Table_Increment      => Choices_Increment,
                       Table_Name           => "Prj.Strt.Choices");
   --  Used to store the case labels and check that there is no duplicate.

   package Choice_Lasts is
      new Table.Table (Table_Component_Type => Choice_Node_Id,
                       Table_Index_Type     => Nat,
                       Table_Low_Bound      => 1,
                       Table_Initial        => 3,
                       Table_Increment      => 3,
                       Table_Name           => "Prj.Strt.Choice_Lasts");
   --  Used to store the indices of the choices in table Choices,
   --  to distinguish nested case constructions.

   Choice_First : Choice_Node_Id := 0;
   --  Index in table Choices of the first case label of the current
   --  case construction.
   --  0 means no current case construction.

   procedure Add (This_String : String_Id);
   --  Add a string to the case label list, indicating that it has not
   --  yet been used.

   procedure External_Reference (External_Value : out Project_Node_Id);
   --  Parse an external reference. Current token is "external".

   procedure Attribute_Reference
     (Reference       : out Project_Node_Id;
      First_Attribute : Attribute_Node_Id;
      Current_Project : Project_Node_Id;
      Current_Package : Project_Node_Id);
   --  Parse an attribute reference. Current token is an apostrophe.

   procedure Terms
     (Term            : out Project_Node_Id;
      Expr_Kind       : in out Variable_Kind;
      Current_Project : Project_Node_Id;
      Current_Package : Project_Node_Id);
   --  Recursive procedure to parse one term or several terms concatenated
   --  using "&".

   ---------
   -- Add --
   ---------

   procedure Add (This_String : String_Id) is
   begin
      Choices.Increment_Last;
      Choices.Table (Choices.Last) :=
        (The_String   => This_String,
         Already_Used => False);
   end Add;

   -------------------------
   -- Attribute_Reference --
   -------------------------

   procedure Attribute_Reference
     (Reference       : out Project_Node_Id;
      First_Attribute : Attribute_Node_Id;
      Current_Project : Project_Node_Id;
      Current_Package : Project_Node_Id)
   is
      Current_Attribute : Attribute_Node_Id := First_Attribute;

   begin
      Reference :=  Default_Project_Node (Of_Kind => N_Attribute_Reference);
      Set_Location_Of (Reference, To => Token_Ptr);
      Scan; --  past apostrophe
      Expect (Tok_Identifier, "Identifier");

      if Token = Tok_Identifier then
         Set_Name_Of (Reference, To => Token_Name);

         while Current_Attribute /= Empty_Attribute
           and then
             Attributes.Table (Current_Attribute).Name /= Token_Name
         loop
            Current_Attribute := Attributes.Table (Current_Attribute).Next;
         end loop;

         if Current_Attribute = Empty_Attribute then
            Error_Msg_Name_1 := Token_Name;
            Error_Msg ("unknown attribute %", Token_Ptr);
            Reference := Empty_Node;

         else
            Set_Project_Node_Of (Reference, To => Current_Project);
            Set_Package_Node_Of (Reference, To => Current_Package);
            Set_Expression_Kind_Of
              (Reference, To => Attributes.Table (Current_Attribute).Kind_1);
            Set_Case_Insensitive
              (Reference, To => Attributes.Table (Current_Attribute).Kind_2 =
                                          Case_Insensitive_Associative_Array);
            Scan;

            if Attributes.Table (Current_Attribute).Kind_2 /= Single then
               Expect (Tok_Left_Paren, "(");

               if Token = Tok_Left_Paren then
                  Scan;
                  Expect (Tok_String_Literal, "literal string");

                  if Token = Tok_String_Literal then
                     Set_Associative_Array_Index_Of
                       (Reference, To => Strval (Token_Node));
                     Scan;
                     Expect (Tok_Right_Paren, ")");

                     if Token = Tok_Right_Paren then
                        Scan;
                     end if;
                  end if;
               end if;
            end if;
         end if;
      end if;
   end Attribute_Reference;

   ---------------------------
   -- End_Case_Construction --
   ---------------------------

   procedure End_Case_Construction is
   begin
      if Choice_Lasts.Last = 1 then
         Choice_Lasts.Set_Last (0);
         Choices.Set_Last (First_Choice_Node_Id);
         Choice_First := 0;

      elsif Choice_Lasts.Last = 2 then
         Choice_Lasts.Set_Last (1);
         Choices.Set_Last (Choice_Lasts.Table (1));
         Choice_First := 1;

      else
         Choice_Lasts.Decrement_Last;
         Choices.Set_Last (Choice_Lasts.Table (Choice_Lasts.Last));
         Choice_First := Choice_Lasts.Table (Choice_Lasts.Last - 1) + 1;
      end if;
   end End_Case_Construction;

   ------------------------
   -- External_Reference --
   ------------------------

   procedure External_Reference (External_Value : out Project_Node_Id) is
      Field_Id : Project_Node_Id := Empty_Node;

   begin
      External_Value :=
        Default_Project_Node (Of_Kind       => N_External_Value,
                              And_Expr_Kind => Single);
      Set_Location_Of (External_Value, To => Token_Ptr);

      --  The current token is External

      --  Get the left parenthesis

      Scan;
      Expect (Tok_Left_Paren, "(");

      --  Scan past the left parenthesis

      if Token = Tok_Left_Paren then
         Scan;
      end if;

      --  Get the name of the external reference

      Expect (Tok_String_Literal, "literal string");

      if Token = Tok_String_Literal then
         Field_Id :=
           Default_Project_Node (Of_Kind       => N_Literal_String,
                                 And_Expr_Kind => Single);
         Set_String_Value_Of (Field_Id, To => Strval (Token_Node));
         Set_External_Reference_Of (External_Value, To => Field_Id);

         --  Scan past the first argument

         Scan;

         case Token is

            when Tok_Right_Paren =>

               --  Scan past the right parenthesis
               Scan;

            when Tok_Comma =>

               --  Scan past the comma

               Scan;

               Expect (Tok_String_Literal, "literal string");

               --  Get the default

               if Token = Tok_String_Literal then
                  Field_Id :=
                    Default_Project_Node (Of_Kind       => N_Literal_String,
                                          And_Expr_Kind => Single);
                  Set_String_Value_Of (Field_Id, To => Strval (Token_Node));
                  Set_External_Default_Of (External_Value, To => Field_Id);
                  Scan;
                  Expect (Tok_Right_Paren, ")");
               end if;

               --  Scan past the right parenthesis
               if Token = Tok_Right_Paren then
                  Scan;
               end if;

            when others =>
               Error_Msg ("',' or ')' expected", Token_Ptr);
         end case;
      end if;
   end External_Reference;

   -----------------------
   -- Parse_Choice_List --
   -----------------------

   procedure Parse_Choice_List (First_Choice : out Project_Node_Id) is
      Current_Choice : Project_Node_Id := Empty_Node;
      Next_Choice    : Project_Node_Id := Empty_Node;
      Choice_String  : String_Id       := No_String;
      Found          : Boolean         := False;

   begin
      First_Choice :=
        Default_Project_Node (Of_Kind       => N_Literal_String,
                              And_Expr_Kind => Single);
      Current_Choice := First_Choice;

      loop
         Expect (Tok_String_Literal, "literal string");
         exit when Token /= Tok_String_Literal;
         Set_Location_Of (Current_Choice, To => Token_Ptr);
         Choice_String := Strval (Token_Node);
         Set_String_Value_Of (Current_Choice, To => Choice_String);

         Found := False;
         for Choice in Choice_First .. Choices.Last loop
            if String_Equal (Choices.Table (Choice).The_String,
                             Choice_String)
            then
               Found := True;

               if Choices.Table (Choice).Already_Used then
                  String_To_Name_Buffer (Choice_String);
                  Error_Msg_Name_1 := Name_Find;
                  Error_Msg ("duplicate case label {", Token_Ptr);
               else
                  Choices.Table (Choice).Already_Used := True;
               end if;

               exit;
            end if;
         end loop;

         if not Found then
            String_To_Name_Buffer (Choice_String);
            Error_Msg_Name_1 := Name_Find;
            Error_Msg ("illegal case label {", Token_Ptr);
         end if;

         Scan;

         if Token = Tok_Vertical_Bar then
            Next_Choice :=
              Default_Project_Node (Of_Kind       => N_Literal_String,
                                    And_Expr_Kind => Single);
            Set_Next_Literal_String (Current_Choice, To => Next_Choice);
            Current_Choice := Next_Choice;
            Scan;
         else
            exit;
         end if;
      end loop;
   end Parse_Choice_List;

   ----------------------
   -- Parse_Expression --
   ----------------------

   procedure Parse_Expression
     (Expression      : out Project_Node_Id;
      Current_Project : Project_Node_Id;
      Current_Package : Project_Node_Id)
   is
      First_Term      : Project_Node_Id := Empty_Node;
      Expression_Kind : Variable_Kind := Undefined;

   begin
      Expression := Default_Project_Node (Of_Kind => N_Expression);
      Set_Location_Of (Expression, To => Token_Ptr);
      Terms (Term            => First_Term,
             Expr_Kind       => Expression_Kind,
             Current_Project => Current_Project,
             Current_Package => Current_Package);
      Set_First_Term (Expression, To => First_Term);
      Set_Expression_Kind_Of (Expression, To => Expression_Kind);
   end Parse_Expression;

   ----------------------------
   -- Parse_String_Type_List --
   ----------------------------

   procedure Parse_String_Type_List (First_String : out Project_Node_Id) is
      Last_String  : Project_Node_Id := Empty_Node;
      Next_String  : Project_Node_Id := Empty_Node;
      String_Value : String_Id := No_String;

   begin
      First_String :=
        Default_Project_Node (Of_Kind       => N_Literal_String,
                              And_Expr_Kind => Single);
      Last_String := First_String;

      loop
         Expect (Tok_String_Literal, "literal string");
         exit when Token /= Tok_String_Literal;
         String_Value := Strval (Token_Node);
         Set_String_Value_Of (Last_String, To => String_Value);
         Set_Location_Of (Last_String, To => Token_Ptr);

         declare
            Current : Project_Node_Id := First_String;

         begin
            while Current /= Last_String loop
               if String_Equal (String_Value_Of (Current), String_Value) then
                  String_To_Name_Buffer (String_Value);
                  Error_Msg_Name_1 := Name_Find;
                  Error_Msg ("duplicate value { in type", Token_Ptr);
                  exit;
               end if;

               Current := Next_Literal_String (Current);
            end loop;
         end;

         Scan;

         if Token /= Tok_Comma then
            exit;

         else
            Next_String :=
              Default_Project_Node (Of_Kind       => N_Literal_String,
                                    And_Expr_Kind => Single);
            Set_Next_Literal_String (Last_String, To => Next_String);
            Last_String := Next_String;
            Scan;
         end if;
      end loop;
   end Parse_String_Type_List;

   ------------------------------
   -- Parse_Variable_Reference --
   ------------------------------

   procedure Parse_Variable_Reference
     (Variable        : out Project_Node_Id;
      Current_Project : Project_Node_Id;
      Current_Package : Project_Node_Id)
   is
      The_Names        : Names;
      Last_Name        : Name_Range := 0;
      Current_Variable : Project_Node_Id := Empty_Node;

      The_Package : Project_Node_Id := Current_Package;
      The_Project : Project_Node_Id := Current_Project;

      Specified_Project : Project_Node_Id   := Empty_Node;
      Specified_Package : Project_Node_Id   := Empty_Node;
      Look_For_Variable : Boolean           := True;
      First_Attribute   : Attribute_Node_Id := Empty_Attribute;
      Variable_Name     : Name_Id;

   begin
      for Index in The_Names'Range loop
         Expect (Tok_Identifier, "identifier");

         if Token /= Tok_Identifier then
            Look_For_Variable := False;
            exit;
         end if;

         Last_Name := Last_Name + 1;
         The_Names (Last_Name) :=
           (Name     => Token_Name,
            Location => Token_Ptr);
         Scan;
         exit when Token /= Tok_Dot;
         Scan;
      end loop;

      if Look_For_Variable then
         if Token = Tok_Apostrophe then

            --  Attribute reference

            case Last_Name is
               when 0 =>

                  --  Cannot happen

                  null;

               when 1 =>
                  for Index in Package_First .. Package_Attributes.Last loop
                     if Package_Attributes.Table (Index).Name =
                                                      The_Names (1).Name
                     then
                        First_Attribute :=
                          Package_Attributes.Table (Index).First_Attribute;
                        exit;
                     end if;
                  end loop;

                  if First_Attribute /= Empty_Attribute then
                     The_Package := First_Package_Of (Current_Project);
                     while The_Package /= Empty_Node
                       and then Name_Of (The_Package) /= The_Names (1).Name
                     loop
                        The_Package := Next_Package_In_Project (The_Package);
                     end loop;

                     if The_Package = Empty_Node then
                        Error_Msg_Name_1 := The_Names (1).Name;
                        Error_Msg ("package % not yet defined",
                                   The_Names (1).Location);
                     end if;

                  else
                     First_Attribute := Attribute_First;
                     The_Package     := Empty_Node;

                     declare
                        The_Project_Name_And_Node :
                          constant Tree_Private_Part.Project_Name_And_Node :=
                            Tree_Private_Part.Projects_Htable.Get
                                                          (The_Names (1).Name);

                        use Tree_Private_Part;

                     begin
                        if The_Project_Name_And_Node =
                                   Tree_Private_Part.No_Project_Name_And_Node
                        then
                           Error_Msg_Name_1 := The_Names (1).Name;
                           Error_Msg ("unknown project %",
                                      The_Names (1).Location);
                        else
                           The_Project := The_Project_Name_And_Node.Node;
                        end if;
                     end;
                  end if;

               when 2 =>
                  declare
                     With_Clause : Project_Node_Id :=
                                     First_With_Clause_Of (Current_Project);

                  begin
                     while With_Clause /= Empty_Node loop
                        The_Project := Project_Node_Of (With_Clause);
                        exit when Name_Of (The_Project) = The_Names (1).Name;
                        With_Clause := Next_With_Clause_Of (With_Clause);
                     end loop;

                     if With_Clause = Empty_Node then
                        Error_Msg_Name_1 := The_Names (1).Name;
                        Error_Msg ("unknown project %",
                                   The_Names (1).Location);
                        The_Project := Empty_Node;
                        The_Package := Empty_Node;
                        First_Attribute := Attribute_First;

                     else
                        The_Package := First_Package_Of (The_Project);
                        while The_Package /= Empty_Node
                          and then Name_Of (The_Package) /= The_Names (2).Name
                        loop
                           The_Package :=
                             Next_Package_In_Project (The_Package);
                        end loop;

                        if The_Package = Empty_Node then
                           Error_Msg_Name_1 := The_Names (2).Name;
                           Error_Msg_Name_2 := The_Names (1).Name;
                           Error_Msg ("package % not declared in project %",
                                      The_Names (2).Location);
                           First_Attribute := Attribute_First;

                        else
                           First_Attribute :=
                             Package_Attributes.Table
                             (Package_Id_Of (The_Package)).First_Attribute;
                        end if;
                     end if;
                  end;

               when 3 =>
                  Error_Msg
                    ("too many single names for an attribute reference",
                     The_Names (1).Location);
                  Scan;
                  Variable := Empty_Node;
                  return;
            end case;

            Attribute_Reference
              (Variable,
               Current_Project => The_Project,
               Current_Package => The_Package,
               First_Attribute => First_Attribute);
            return;
         end if;
      end if;

      Variable :=
        Default_Project_Node (Of_Kind => N_Variable_Reference);

      if Look_For_Variable then
         case Last_Name is
            when 0 =>

               --  Cannot happen

               null;

            when 1 =>
               Set_Name_Of (Variable, To => The_Names (1).Name);

            --  Header comment needed ???

            when 2 =>
               Set_Name_Of (Variable, To => The_Names (2).Name);
               The_Package := First_Package_Of (Current_Project);

               while The_Package /= Empty_Node
                 and then Name_Of (The_Package) /= The_Names (1).Name
               loop
                  The_Package := Next_Package_In_Project (The_Package);
               end loop;

               if The_Package /= Empty_Node then
                  Specified_Package := The_Package;
                  The_Project := Empty_Node;

               else
                  declare
                     With_Clause : Project_Node_Id :=
                                     First_With_Clause_Of (Current_Project);

                  begin
                     while With_Clause /= Empty_Node loop
                        The_Project := Project_Node_Of (With_Clause);
                        exit when Name_Of (The_Project) = The_Names (1).Name;
                        With_Clause := Next_With_Clause_Of (With_Clause);
                     end loop;

                     if With_Clause = Empty_Node then
                        The_Project :=
                          Modified_Project_Of
                                 (Project_Declaration_Of (Current_Project));

                        if The_Project /= Empty_Node
                          and then
                            Name_Of (The_Project) /= The_Names (1).Name
                        then
                           The_Project := Empty_Node;
                        end if;
                     end if;

                     if The_Project = Empty_Node then
                        Error_Msg_Name_1 := The_Names (1).Name;
                        Error_Msg ("unknown package or project %",
                                   The_Names (1).Location);
                        Look_For_Variable := False;
                     else
                        Specified_Project := The_Project;
                     end if;
                  end;
               end if;

            --  Header comment needed ???

            when 3 =>
               Set_Name_Of (Variable, To => The_Names (3).Name);

               declare
                  With_Clause : Project_Node_Id :=
                                  First_With_Clause_Of (Current_Project);

               begin
                  while With_Clause /= Empty_Node loop
                     The_Project := Project_Node_Of (With_Clause);
                     exit when Name_Of (The_Project) = The_Names (1).Name;
                     With_Clause := Next_With_Clause_Of (With_Clause);
                  end loop;

                  if With_Clause = Empty_Node then
                     The_Project :=
                       Modified_Project_Of
                          (Project_Declaration_Of (Current_Project));

                     if The_Project /= Empty_Node
                       and then Name_Of (The_Project) /= The_Names (1).Name
                     then
                        The_Project := Empty_Node;
                     end if;
                  end if;

                  if The_Project = Empty_Node then
                     Error_Msg_Name_1 := The_Names (1).Name;
                     Error_Msg ("unknown package or project %",
                                The_Names (1).Location);
                     Look_For_Variable := False;

                  else
                     Specified_Project := The_Project;
                     The_Package := First_Package_Of (The_Project);

                     while The_Package /= Empty_Node
                       and then Name_Of (The_Package) /= The_Names (2).Name
                     loop
                        The_Package := Next_Package_In_Project (The_Package);
                     end loop;

                     if The_Package = Empty_Node then
                        Error_Msg_Name_1 := The_Names (2).Name;
                        Error_Msg ("unknown package %",
                                   The_Names (2).Location);
                        Look_For_Variable := False;

                     else
                        Specified_Package := The_Package;
                        The_Project := Empty_Node;
                     end if;
                  end if;
               end;

         end case;
      end if;

      if Look_For_Variable then
         Variable_Name := Name_Of (Variable);
         Set_Project_Node_Of (Variable, To => Specified_Project);
         Set_Package_Node_Of (Variable, To => Specified_Package);

         if The_Package /= Empty_Node then
            Current_Variable := First_Variable_Of (The_Package);

            while Current_Variable /= Empty_Node
              and then
              Name_Of (Current_Variable) /= Variable_Name
            loop
               Current_Variable := Next_Variable (Current_Variable);
            end loop;
         end if;

         if Current_Variable = Empty_Node
           and then The_Project /= Empty_Node
         then
            Current_Variable := First_Variable_Of (The_Project);
            while Current_Variable /= Empty_Node
              and then Name_Of (Current_Variable) /= Variable_Name
            loop
               Current_Variable := Next_Variable (Current_Variable);
            end loop;
         end if;

         if Current_Variable = Empty_Node then
            Error_Msg_Name_1 := Variable_Name;
            Error_Msg ("unknown variable %", The_Names (Last_Name).Location);
         end if;
      end if;

      if Current_Variable /= Empty_Node then
         Set_Expression_Kind_Of
           (Variable, To => Expression_Kind_Of (Current_Variable));

         if Kind_Of (Current_Variable) = N_Typed_Variable_Declaration then
            Set_String_Type_Of
              (Variable, To => String_Type_Of (Current_Variable));
         end if;
      end if;

      if Token = Tok_Left_Paren then
         Error_Msg ("\variables cannot be associative arrays", Token_Ptr);
         Scan;
         Expect (Tok_String_Literal, "literal string");

         if Token = Tok_String_Literal then
            Scan;
            Expect (Tok_Right_Paren, ")");

            if Token = Tok_Right_Paren then
               Scan;
            end if;
         end if;
      end if;
   end Parse_Variable_Reference;

   ---------------------------------
   -- Start_New_Case_Construction --
   ---------------------------------

   procedure Start_New_Case_Construction (String_Type  : Project_Node_Id) is
      Current_String : Project_Node_Id;

   begin
      if Choice_First = 0 then
         Choice_First := 1;
         Choices.Set_Last (First_Choice_Node_Id);
      else
         Choice_First := Choices.Last + 1;
      end if;

      if String_Type /= Empty_Node then
         Current_String := First_Literal_String (String_Type);

         while Current_String /= Empty_Node loop
            Add (This_String => String_Value_Of (Current_String));
            Current_String := Next_Literal_String (Current_String);
         end loop;
      end if;

      Choice_Lasts.Increment_Last;
      Choice_Lasts.Table (Choice_Lasts.Last) := Choices.Last;

   end Start_New_Case_Construction;

   -----------
   -- Terms --
   -----------

   procedure Terms (Term            : out Project_Node_Id;
                    Expr_Kind       : in out Variable_Kind;
                    Current_Project : Project_Node_Id;
                    Current_Package : Project_Node_Id)
   is
      Next_Term          : Project_Node_Id := Empty_Node;
      Term_Id            : Project_Node_Id := Empty_Node;
      Current_Expression : Project_Node_Id := Empty_Node;
      Next_Expression    : Project_Node_Id := Empty_Node;
      Current_Location   : Source_Ptr      := No_Location;
      Reference          : Project_Node_Id := Empty_Node;

   begin
      Term := Default_Project_Node (Of_Kind => N_Term);
      Set_Location_Of (Term, To => Token_Ptr);

      case Token is

         when Tok_Left_Paren =>
            case Expr_Kind is
               when Undefined =>
                  Expr_Kind := List;
               when List =>
                  null;
               when Single =>
                  Expr_Kind := List;
                  Error_Msg
                    ("literal string list cannot appear in a string",
                     Token_Ptr);
            end case;

            Term_Id := Default_Project_Node
              (Of_Kind => N_Literal_String_List,
               And_Expr_Kind => List);
            Set_Current_Term (Term, To => Term_Id);
            Set_Location_Of (Term, To => Token_Ptr);

            Scan;
            if Token = Tok_Right_Paren then
               Scan;

            else
               loop
                  Current_Location := Token_Ptr;
                  Parse_Expression (Expression      => Next_Expression,
                                    Current_Project => Current_Project,
                                    Current_Package => Current_Package);

                  if Expression_Kind_Of (Next_Expression) = List then
                     Error_Msg ("single expression expected",
                                Current_Location);
                  end if;

                  if Current_Expression = Empty_Node then
                     Set_First_Expression_In_List
                       (Term_Id, To => Next_Expression);
                  else
                     Set_Next_Expression_In_List
                       (Current_Expression, To => Next_Expression);
                  end if;

                  Current_Expression := Next_Expression;
                  exit when Token /= Tok_Comma;
                  Scan; -- past the comma
               end loop;

               Expect (Tok_Right_Paren, "(");

               if Token = Tok_Right_Paren then
                  Scan;
               end if;
            end if;

         when Tok_String_Literal =>
            if Expr_Kind = Undefined then
               Expr_Kind := Single;
            end if;

            Term_Id := Default_Project_Node (Of_Kind => N_Literal_String);
            Set_Current_Term (Term, To => Term_Id);
            Set_String_Value_Of (Term_Id, To => Strval (Token_Node));

            Scan;

         when Tok_Identifier =>
            Current_Location := Token_Ptr;
            Parse_Variable_Reference
              (Variable        => Reference,
               Current_Project => Current_Project,
               Current_Package => Current_Package);
            Set_Current_Term (Term, To => Reference);

            if Reference /= Empty_Node then
               if Expr_Kind = Undefined then
                  Expr_Kind := Expression_Kind_Of (Reference);

               elsif Expr_Kind = Single
                 and then Expression_Kind_Of (Reference) = List
               then
                  Expr_Kind := List;
                  Error_Msg
                    ("list variable cannot appear in single string expression",
                     Current_Location);
               end if;
            end if;

         when Tok_Project =>
            Current_Location := Token_Ptr;
            Scan;
            Expect (Tok_Apostrophe, "'");

            if Token = Tok_Apostrophe then
               Attribute_Reference
                 (Reference       => Reference,
                  First_Attribute => Prj.Attr.Attribute_First,
                  Current_Project => Current_Project,
                  Current_Package => Empty_Node);
               Set_Current_Term (Term, To => Reference);
            end if;

            if Reference /= Empty_Node then
               if Expr_Kind = Undefined then
                  Expr_Kind := Expression_Kind_Of (Reference);

               elsif Expr_Kind = Single
                 and then Expression_Kind_Of (Reference) = List
               then
                  Error_Msg
                    ("lists cannot appear in single string expression",
                     Current_Location);
               end if;
            end if;

         when Tok_External =>
            if Expr_Kind = Undefined then
               Expr_Kind := Single;
            end if;

            External_Reference (External_Value => Reference);
            Set_Current_Term (Term, To => Reference);

         when others =>
            Error_Msg ("cannot be part of an expression", Token_Ptr);
            Term := Empty_Node;
            return;
      end case;

      if Token = Tok_Ampersand then
         Scan;

         Terms (Term            => Next_Term,
                Expr_Kind       => Expr_Kind,
                Current_Project => Current_Project,
                Current_Package => Current_Package);
         Set_Next_Term (Term, To => Next_Term);

      end if;

   end Terms;

end Prj.Strt;
