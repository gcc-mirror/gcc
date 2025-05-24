-- CXA4021.A
--
--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687,
--     F08630-91-C-0015, and DCA100-97-D-0025, the U.S. Government obtained 
--     unlimited rights in the software and documentation contained herein.
--     Unlimited rights are defined in DFAR 252.227-7013(a)(19).  By making 
--     this public release, the Government intends to confer upon all 
--     recipients unlimited rights  equal to those held by the Government.  
--     These rights include rights to use, duplicate, release or disclose the 
--     released technical data and computer software in whole or in part, in 
--     any manner and for any purpose whatsoever, and to have or permit others 
--     to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED 
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE 
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
--
-- OBJECTIVE:
--      Check that the subprograms defined in package 
--      Ada.Strings.Wide_Unbounded are available, and that they produce 
--      correct results. Specifically, check the subprograms Head, Index, 
--      Index_Non_Blank, Insert, Length, Overwrite, Replace_Slice, Slice, 
--      Tail, To_Wide_String, To_Unbounded_Wide_String, "*", "&", 
--      and "=", "<=", ">=".
--      
-- TEST DESCRIPTION:
--      This test demonstrates the uses of many of the subprograms defined
--      in package Ada.Strings.Wide_Unbounded for use with unbounded wide
--      strings.
--      The test attempts to simulate how unbounded wide strings could be used
--      to simulate paragraphs of text.  Modifications could be easily be
--      performed using the provided subprograms (although in this test, the
--      main modification performed was the addition of more text to the
--      string).  One would not have to worry about the formatting of the 
--      paragraph until it was finished and correct in content.  Then, once
--      all required editing is complete, the unbounded strings can be divided
--      up into the appropriate lengths based on particular formatting
--      requirements.  The test then compares the formatted text product
--      with a predefined "finished product".  
--      
--      This test attempts to use a large number of the subprograms provided
--      by package Ada.Strings.Wide_Unbounded.  Often, the processing involved
--      could have been performed more efficiently using a minimum number
--      of the subprograms, in conjunction with loops, etc.  However, for
--      testing purposes, and in the interest of minimizing the number of
--      tests developed, subprogram variety and feature mixing was stressed.
--
--       
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

with Report;
with Ada.Characters.Handling;
with Ada.Strings.Wide_Maps;
with Ada.Strings.Wide_Unbounded;

procedure CXA4021 is

   -- The following two functions are used to translate character and string
   -- values to "Wide" values.  They will be applied to all the Wide_Bounded
   -- subprogram character and string parameters to simulate the use of non-
   -- character Wide_Characters and Wide_Strings in actual practice.
   -- Note: These functions do not actually return "equivalent" wide
   --       characters to their character inputs, just "non-character"
   --       wide characters.

   function Equiv (Ch : Character) return Wide_Character is
      C : Character := Ch;
   begin
      if Ch = ' ' then
         return Ada.Characters.Handling.To_Wide_Character(C);
      else
         return Wide_Character'Val(Character'Pos(Ch) + 
                Character'Pos(Character'Last) + 1);
      end if;
   end Equiv;


   function Equiv (Str : String) return Wide_String is
      WS : Wide_String(Str'First..Str'Last);
   begin
      for i in Str'First..Str'Last loop
         WS(i) := Equiv(Str(i));
      end loop;
      return WS;
   end Equiv;

begin

   Report.Test ("CXA4021", "Check that the subprograms defined in "     &
                           "package Ada.Strings.Wide_Unbounded are "    &
                           "available, and that they produce correct "  &
                           "results");

   Test_Block:
   declare

      package ASW renames Ada.Strings.Wide_Unbounded;
      use type ASW.Unbounded_Wide_String;
      use Ada.Strings;

      Pamphlet_Paragraph_Count : constant :=  2;
      Lines                    : constant :=  4;
      Line_Length              : constant := 40;

      type Document_Type is array (Positive range <>) 
        of ASW.Unbounded_Wide_String;

      type Camera_Ready_Copy_Type is array (1..Lines) 
        of Wide_String (1..Line_Length);

      Pamphlet            : Document_Type (1..Pamphlet_Paragraph_Count);

      Camera_Ready_Copy   : Camera_Ready_Copy_Type := 
                              (others => (others => Ada.Strings.Wide_Space));

      TC_Finished_Product : Camera_Ready_Copy_Type :=
                   ( 1 => Equiv("Ada is a programming language designed  "),
                     2 => Equiv("to support long-lived, reliable software"),
                     3 => Equiv(" systems.                               "),
                     4 => Equiv("Go with Ada!                            "));

      -----


      procedure Enter_Text_Into_Document (Document : in out Document_Type) is
      begin

         -- Fill in both "paragraphs" of the document.  Each unbounded wide 
         -- string functions as an individual paragraph, containing an 
         -- unspecified number of characters.
         -- Use a variety of different unbounded wide string subprograms to
         -- load the data.

         Document(1) := 
           ASW.To_Unbounded_Wide_String(Equiv("Ada is a language"));

         -- Insert the word "programming" prior to "language".
         Document(1) := 
           ASW.Insert(Document(1), 
                      ASW.Index(Document(1),
                                Equiv("language")),
                      ASW.To_Wide_String(Equiv("progra") &       -- Wd Str &
                                         ASW."*"(2,Equiv('m')) & -- Wd Unbd &
                                         Equiv("ing ")));        -- Wd Str


         -- Overwrite the word "language" with "language" + additional text.
         Document(1) :=
           ASW.Overwrite(Document(1),
                         ASW.Index(Document(1),
                                   ASW.To_Wide_String(
                                     ASW.Tail(Document(1), 8, Equiv(' '))),
                                   Ada.Strings.Backward),
                         Equiv("language designed to support long-lifed"));


         -- Replace the word "lifed" with "lived".
         Document(1) :=
           ASW.Replace_Slice(Document(1),
                             ASW.Index(Document(1), Equiv("lifed")),
                             ASW.Length(Document(1)),
                             Equiv("lived"));


         -- Overwrite the word "lived" with "lived" + additional text.
         Document(1) :=
           ASW.Overwrite(Document(1),
                         ASW.Index(Document(1),
                                   ASW.To_Wide_String
                                     (ASW.Tail(Document(1), 5, Equiv(' '))),
                                   Ada.Strings.Backward),
                         Equiv("lived, reliable software systems."));
                              
                              
         -- Use several of the overloaded versions of "&" to form this
         -- unbounded wide string.

         Document(2) := Equiv('G')                                  & 
                        ASW.To_Unbounded_Wide_String(Equiv("o "))   & 
                        ASW.To_Unbounded_Wide_String(Equiv("with")) &
                        Equiv(' ')                                  & 
                        Equiv("Ada!");

      end Enter_Text_Into_Document;


      -----


      procedure Create_Camera_Ready_Copy 
                  (Document    : in     Document_Type;
                   Camera_Copy :    out Camera_Ready_Copy_Type) is
      begin
         -- Break the unbounded wide strings into fixed lengths.

         -- Search the first unbounded wide string for portions of text that
         -- are less than or equal to the length of a wide string in the 
         -- Camera_Ready_Copy_Type object.  

         Camera_Copy(1) :=               -- Take characters 1-39,
           ASW.Slice(Document(1),        -- and append a blank space.
                     1,
                     ASW.Index(ASW.To_Unbounded_Wide_String
                                 (ASW.Slice(Document(1),
                                            1,
                                            Line_Length)),
                               Ada.Strings.Wide_Maps.To_Set(Equiv(' ')),
                               Ada.Strings.Inside,
                               Ada.Strings.Backward)) & Equiv(' ');

         Camera_Copy(2) :=                 -- Take characters 40-79.
           ASW.Slice(Document(1),
                     40,
                     (ASW.Index_Non_Blank              -- Should return 79
                        (ASW.To_Unbounded_Wide_String
                           (ASW.Slice(Document(1),     -- Slice (40..79)
                                      40,
                                      79)),
                         Ada.Strings.Backward) + 39)); -- Increment since
                                                       -- this slice starts
                                                       -- at 40.

         Camera_Copy(3)(1..9) := ASW.Slice(Document(1), -- Characters 80-88
                                           80,
                                           ASW.Length(Document(1))); 
                                

         -- Break the second unbounded wide string into the appropriate
         -- length.  It is only twelve characters in length, so the entire 
         -- unbounded wide string will be placed on one string of the output 
         -- object.

         Camera_Copy(4)(1..ASW.Length(Document(2))) :=
           ASW.To_Wide_String(ASW.Head(Document(2),
                                       ASW.Length(Document(2))));

      end Create_Camera_Ready_Copy;


      -----


      function Valid_Proofread (Draft, Master : Camera_Ready_Copy_Type) 
        return Boolean is
      begin

         -- Evaluate wide strings for equality, using the operators defined
         -- in package Ada.Strings.Wide_Unbounded.  The less than/greater 
         -- than or equal comparisons should evaluate to "equals => True".

         if ASW.To_Unbounded_Wide_String(Draft(1))   =     -- "="(WUnb,WUnb)
            ASW.To_Unbounded_Wide_String(Master(1))   and  
            ASW.To_Unbounded_Wide_String(Draft(2))  <=     -- "<="(WUnb,WUnb)
            ASW.To_Unbounded_Wide_String(Master(2))   and
            ASW.To_Unbounded_Wide_String(Draft(3))  >=     -- ">="(WUnb,WUnb)
            ASW.To_Unbounded_Wide_String(Master(3))   and
            ASW.To_Unbounded_Wide_String(Draft(4))   =     -- "="(WUnb,WUnb)
            ASW.To_Unbounded_Wide_String(Master(4))
         then
            return True;
         else
            return False;
         end if;

      end Valid_Proofread;


      -----


   begin

      -- Enter text into the unbounded wide string paragraphs of the document.

      Enter_Text_Into_Document (Pamphlet);


      -- Reformat the unbounded wide strings into fixed wide string format.

      Create_Camera_Ready_Copy (Document    => Pamphlet,
                                Camera_Copy => Camera_Ready_Copy);


      -- Verify the conversion process.

      if not Valid_Proofread (Draft  => Camera_Ready_Copy,
                              Master => TC_Finished_Product)
      then
         Report.Failed ("Incorrect unbounded wide string processing result");
      end if;


   exception
      when others => Report.Failed ("Exception raised in Test_Block");
   end Test_Block;


   Report.Result;

end CXA4021;
