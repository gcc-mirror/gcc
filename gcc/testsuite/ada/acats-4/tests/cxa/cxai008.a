-- CXAI008.A
--
--                             Grant of Unlimited Rights
--
--     The Ada Conformity Assessment Authority (ACAA) holds unlimited
--     rights in the software and documentation contained herein. Unlimited
--     rights are the same as those granted by the U.S. Government for older
--     parts of the Ada Conformity Assessment Test Suite, and are defined
--     in DFAR 252.227-7013(a)(19). By making this public release, the ACAA
--     intends to confer upon all recipients unlimited rights equal to those
--     held by the ACAA. These rights include rights to use, duplicate,
--     release or disclose the released technical data and computer software
--     in whole or in part, in any manner and for any purpose whatsoever, and
--     to have or permit others to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS. THE ACAA MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--
--                                     Notice
--
--     The ACAA has created and maintains the Ada Conformity Assessment Test
--     Suite for the purpose of conformity assessments conducted in accordance
--     with the International Standard ISO/IEC 18009 - Ada: Conformity
--     assessment of a language processor. This test suite should not be used
--     to make claims of conformance unless used in accordance with
--     ISO/IEC 18009 and any applicable ACAA procedures.
--*
--
-- OBJECTIVE:
--      Check basic operations in package
--      Ada.Containers.Indefinite_Multiway_Trees.
--
-- TEST DESCRIPTION:
--      This test parses an XML string into a multi-way tree.
--
-- APPLICABILITY CRITERIA:
--      This test is applicable to all implementations.
--
--
-- CHANGE HISTORY:
--      15 Jan 11   STT     Ada Rapporteur Group January 2012 Review
--      27 Mar 14   RLB     Created ACATS 4.0 version, renamed test, improved
--                          objective.

with Ada.Containers.Indefinite_Multiway_Trees,
     Report,
     Ada.Exceptions;

procedure CXAI008 is

   type String_Ptr is access all String;

   package Dom is
      type Node_Kinds is
        (Element_Kind,
         Attribute_Kind,
         Cdata_Section_Kind,
         Entity_Reference_Kind,
         Entity_Kind,
         Text_Kind,
         Comment_Kind);

      type Node_Rec (Kind : Node_Kinds) is record
         case Kind is
            when Element_Kind =>
               Elem_Name       : String_Ptr;

            when Attribute_Kind =>
               Attr_Name       : String_Ptr;
               Attr_Value      : String_Ptr;
               Specified       : Boolean := False;

            when Text_Kind =>
               Text : String_Ptr;

            when Cdata_Section_Kind =>
               Cdata : String_Ptr;

            when Entity_Reference_Kind =>
               Entity_Reference_Name : String_Ptr;

            when Entity_Kind =>
               Entity_Name : String_Ptr;

            when Comment_Kind =>
               Comment : String_Ptr;

         end case;
      end record;

      package Node_Tree is new Ada.Containers.Indefinite_Multiway_Trees
        (Element_Type => Node_Rec);

   end Dom;

   use Dom;

   XML_Syntax_Error : exception;

   procedure Parse_Subtree
     (Input : in String;
      Start : in out Positive;
      Tree : in out Node_Tree.Tree;
      Parent : in Node_Tree.Cursor;
      End_Of_Children : out Boolean) is
      -- Parse Input starting at Start, create an appropriate subtree,
      -- and append it as a child to the designated parent.
      -- Update Start to point after the parsed input.
      -- If the input is an end tag, then End_Of_Children is set
      -- to True and Start is updated to point to the start of the end tag.
      -- If the input is at the end of the string, then End_Of_Children
      -- is set to True and Start is updated to point past the end of the
      -- string.
      Within_Start_Tag : Boolean := False;
      Element_Name : String_Ptr;
      Element_Cursor : Node_Tree.Cursor;

      procedure Absorb(Match : String) is
         -- Require a match to specified string.
         -- Allow leading white space.
         -- If Match = "", then this just absorbs white space.
         Len : Natural := 0;
      begin
         while Start <= Input'Last loop
            declare
               C : constant Character := Input(Start);
            begin
               if Len = 0 and then (C = ' ' or else C = ASCII.HT) then
                  -- Absorb leading spaces
                  null;
               else
                  exit when Len = Match'Length;
                  if C /= Match(Match'First + Len) then
                     Report.Comment("Expecting " & Match(Match'First + Len) &
                         ", found " & C);
                     raise XML_Syntax_Error;
                  end if;
                  Len := Len + 1;
               end if;
               Start := Start + 1;
            end;
         end loop;
-- Report.Comment("Absorbed """ & Match & '"');
      end Absorb;

      function Parse_Name return String_Ptr is
         -- Return alphanumeric name; ignore leading white space
         Chars : String(1..1000);
         Len : Natural := 0;
      begin
         while Start <= Input'Last loop
            declare
               C : constant Character := Input(Start);
            begin
               case C is
                  when ' ' | ASCII.HT =>
                     exit when Len > 0;
                  when 'a'..'z' | 'A'..'Z' | '_' | '0'..'9' =>
                     Len := Len + 1;
                     Chars(Len) := C;
                  when others =>
                     exit when Len > 0;
                     Report.Comment("Expected alphanumeric name, found " & C);
                     raise XML_Syntax_Error;
               end case;
               Start := Start + 1;
            end;
         end loop;
         declare
            Result : constant String_Ptr :=  new String'(Chars(1..Len));
         begin
--Report.Comment("Parse_Name returning " & Result.all);
            return Result;
         end;
      end Parse_Name;

      function Parse_Text(End_Char : Character) return String_Ptr is
         -- Return characters up to specified End_Char as a string pointer
         -- "Start" is left pointing at the end character.
         Chars : String(1..1000);
         Len : Natural := 0;
      begin
         while Start <= Input'Last loop
            declare
               C : constant Character := Input(Start);
            begin
               exit when C = End_Char;
               Len := Len + 1;
               Chars(Len) := C;
               Start := Start + 1;
            end;
         end loop;
         declare
            Result : constant String_Ptr :=  new String'(Chars(1..Len));
         begin
--Report.Comment("Parse_Text returning """ & Result.all & '"');
            return Result;
         end;
      end Parse_Text;

      function Parse_Attrib_Value return String_Ptr is
         -- Return characters that make up an attribute value
         -- Ignore leading white space
      begin
         while Start <= Input'Last loop
            declare
               C : constant Character := Input(Start);
            begin
               case C is
                  when ' ' | ASCII.HT =>
                     Start := Start + 1;
                  when 'a'..'z' | 'A' .. 'Z' | '0'..'9' =>
                     return Parse_Name;
                  when '"' | ''' =>
                     Start := Start + 1;
                     declare
                        Result : constant String_Ptr := Parse_Text(C);
                     begin
                        Start := Start + 1;
                        return Result;
                     end;
                  when others =>
                     Report.Comment("Expecting attribute value, found " & C);
                     raise XML_Syntax_Error;
               end case;
            end;
         end loop;
         Report.Comment("Expecting attribute value, found end of input");
         return new String'("");
      end Parse_Attrib_Value;

   begin

      End_Of_Children := False;
      while Start <= Input'Last loop
         declare
            C : constant Character := Input(Start);
         begin
-- Report.Comment("Reading '" & C & ''');
            case C is
               when '<' =>
                  -- Start or end tag
                  if Within_Start_Tag or else Start >= Input'Last then
                     Report.Comment("Unexpected place for '<'");
                     raise XML_Syntax_Error;
                  end if;

                  case Input(Start+1) is
                     when '/' =>
                        -- An end tag
                        End_Of_Children := True;
                        return;

                     when '!' =>
                        -- A comment, Cdata, Doctype, etc.
                        Report.Comment("XML Comment, CData, Doctype NYI");
                        raise XML_Syntax_Error;

                     when 'a' .. 'z' | 'A' .. 'Z' =>
                        -- A start tag
                        Start := Start + 1;
                        Element_Name := Parse_Name;
                        Within_Start_Tag := True;
                        -- Create node and add to tree
                        Node_Tree.Insert_Child(Tree, Parent,
                           Before => Node_Tree.No_Element,
                           New_Item => Node_Rec'(Kind => Element_Kind,
                              Elem_Name => Element_Name),
                           Position => Element_Cursor);


                  -- when '?' => Processing instruction -- TBD

                     when others =>
                        raise XML_Syntax_Error;
                  end case;

               when '"' | ''' =>
                  Start := Start + 1;
                  declare
                     Text : constant String_Ptr := Parse_Text(C);
                  begin
                     Start := Start + 1;
                     if Within_Start_Tag then
                        Report.Comment("Unexpected string within tag " & Text.all);
                        raise XML_Syntax_Error;
                     end if;

                     Node_Tree.Append_Child(Tree, Parent,
                        Node_Rec'(Kind => Text_Kind, Text => Text));
                     return;
                  end;
               when 'a' .. 'z' | 'A' .. 'Z' =>
                  -- Must be an attribute name
                  declare
                     Attr_Name : constant String_Ptr := Parse_Name;
                     Attr_Value : String_Ptr;
                  begin
                     if not Within_Start_Tag then
                        Report.Comment("Unexpected unquoted string " & Attr_Name.all);
                        raise XML_Syntax_Error;
                     end if;

--Report.Comment("Reading attribute " & Attr_Name.all);

                     Absorb("=");
                     Attr_Value := Parse_Attrib_Value;
                     Node_Tree.Append_Child(Tree, Element_Cursor,
                        Node_Rec'(Kind => Attribute_Kind,
                           Attr_Name => Attr_Name, Attr_Value => Attr_Value,
                           Specified => True));
                  end;


               when '/' =>
                  -- Self-contained tag; all done.
--Report.Comment("Self-contained tag " & Element_Name.all);
                  Absorb("/>");
                  return;

               when '>' =>
                  if not Within_Start_Tag then
                     Report.Comment("Unexpected '>'");
                     raise XML_Syntax_Error;
                  end if;
--Report.Comment("Reading nested elements");
                  Within_Start_Tag := False;
                  Start := Start + 1;
                  loop
                     -- Recurse to read children and text elements
                     declare
                        Text : constant String_Ptr := Parse_Text('<');
                        EOC : Boolean := False;
                     begin
                        if Text.all /= "" then
                           -- Append text element, if any
                           Node_Tree.Append_Child(Tree, Element_Cursor,
                              Node_Rec'(Kind => Text_Kind,
                                 Text => Text));
                        end if;
                        -- Append nested element, if any
                        Parse_Subtree(Input, Start, Tree, Element_Cursor, EOC);
                        exit when EOC;
                     end;
                  end loop;

                  -- Reached end tag
                  Absorb("</");
                  declare
                     End_Tag : constant String_Ptr := Parse_Name;
                  begin
                     if End_Tag.all /= Element_Name.all then
                        Report.Comment("End tag " & End_Tag.all &
                           " does not match Start tag " & Element_Name.all);
                        raise XML_Syntax_Error;
                     end if;
                     Absorb(">");
                     -- All done
                     return;
                  end;
               when ' ' | ASCII.HT =>
                  -- ignore white space
                  Start := Start + 1;  -- Advance to next input character

               when others =>
                  raise XML_Syntax_Error;
            end case;

         exception
            when E : others =>
               Report.Failed("Exception " & Ada.Exceptions.Exception_Name(E) & " raised");
               Start := Start + 1;
               raise;
         end;
      end loop;
      End_Of_Children := True;

   end Parse_Subtree;

   Input : constant String := "<ul align='center'><li>Item1</li><li>Item2</li></ul>";
   Start : Positive := Input'First;
   Tree : Node_Tree.Tree;
   End_Of_Children : Boolean := False;

   Tree_Count : Ada.Containers.Count_Type;
   use type Ada.Containers.Count_Type;

   Expected_Count : constant := 7;
begin

   Report.Test ("CXAI008", "Check basic operations in package " &
                            "Ada.Containers.Indefinite_Multiway_Trees");

Report.Comment("Parsing '" & Input & ''');
   -- Parse the input.
   Parse_Subtree(Input, Start, Tree, Tree.Root, End_Of_Children);

   if End_Of_Children then
      Report.Failed("End_Of_Children False after root Parse_Subtree");
   end if;

   -- Now check that the result is as expected:

   Tree_Count := Node_Tree.Node_Count(Tree);

   if Tree_Count /= Expected_Count then
      Report.Failed("Expected node count of" &
         Ada.Containers.Count_Type'Image(Expected_Count) & ", found" &
         Ada.Containers.Count_Type'Image(Tree_Count));
   end if;

   declare
      use Node_Tree;
      Selected_Item_Cursor : constant Cursor :=
         First_Child(Last_Child(First_Child(Tree.Root)));
      Selected_Item : constant Node_Rec := Element(Selected_Item_Cursor);
   begin
      Report.Comment("Selecting item First_Child(Last_Child(First_Child(Root)))");
      Report.Comment("Selected Item is of Kind " & Node_Kinds'Image(Selected_Item.Kind));
      if Selected_Item.Kind /= Text_Kind then
         Report.Failed("Expected Text_Kind");
      elsif Selected_Item.Text.all /= "Item2" then
         Report.Failed("Expected ""Item2"", found """ &
            Selected_Item.Text.all & '"');
      elsif Child_Count(Selected_Item_Cursor) /= 0 then
         Report.Failed("Expected no children, found" &
            Ada.Containers.Count_Type'Image(Child_Count(Selected_Item_Cursor)));
      elsif Child_Count(Parent(Parent(Selected_Item_Cursor))) /= 3 then
         Report.Failed("Expected 3 children of grandparent, found" &
            Ada.Containers.Count_Type'Image
               (Child_Count(Parent(Parent(Selected_Item_Cursor)))));
      else
         Report.Comment("Selected Item has text """ & Selected_Item.Text.all & '"');
      end if;
   end;

   Report.Result;

exception

   when Err:others     =>
      Report.Failed ( "Unexpected exception " &
                      Ada.Exceptions.Exception_Name(Err));
      Report.Result;

end CXAI008;
