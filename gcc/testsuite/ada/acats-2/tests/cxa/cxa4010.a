-- CXA4010.A
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
--      Check that the subprograms defined in package Ada.Strings.Unbounded
--      are available, and that they produce correct results. Specifically, 
--      check the subprograms To_String, To_Unbounded_String, Insert, "&", 
--      "*", Length, Slice, Replace_Slice, Overwrite, Index, Index_Non_Blank,
--      Head, Tail, and "=", "<=", ">=".
--      
-- TEST DESCRIPTION:
--      This test demonstrates the uses of many of the subprograms defined
--      in package Ada.Strings.Unbounded for use with unbounded strings.
--      The test simulates how unbounded strings could be used
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
--      This test uses a large number of the subprograms provided
--      by package Ada.Strings.Unbounded.  Often, the processing involved
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
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;

procedure CXA4010 is
begin

   Report.Test ("CXA4010", "Check that the subprograms defined in "        &
                           "package Ada.Strings.Unbounded are available, " &
                           "and that they produce correct results");

   Test_Block:
   declare

      package ASUnb renames Ada.Strings.Unbounded;
      use type ASUnb.Unbounded_String;
      use Ada.Strings;

      Pamphlet_Paragraph_Count : constant :=  2;
      Lines                    : constant :=  4;
      Line_Length              : constant := 40;

      type Document_Type is array (Positive range <>) 
        of ASUnb.Unbounded_String;

      type Camera_Ready_Copy_Type is array (1..Lines) 
        of String (1..Line_Length);

      Pamphlet            : Document_Type (1..Pamphlet_Paragraph_Count);

      Camera_Ready_Copy   : Camera_Ready_Copy_Type := 
                              (others => (others => Ada.Strings.Space));

      TC_Finished_Product : Camera_Ready_Copy_Type :=
                          ( 1 => "Ada is a programming language designed  ",
                            2 => "to support long-lived, reliable software",
                            3 => " systems.                               ",
                            4 => "Go with Ada!                            ");

      -----


      procedure Enter_Text_Into_Document (Document : in out Document_Type) is
      begin

         -- Fill in both "paragraphs" of the document.  Each unbounded string
         -- functions as an individual paragraph, containing an unspecified
         -- number of characters.
         -- Use a variety of different unbounded string subprograms to load 
         -- the data.

         Document(1) := ASUnb.To_Unbounded_String("Ada is a language");

         -- Insert the word "programming" prior to "language".
         Document(1) := 
           ASUnb.Insert(Document(1), 
                        ASUnb.Index(Document(1),
                                    "language"),
                        ASUnb.To_String("progra" &           -- Str &
                                        ASUnb."*"(2,'m') &   -- Unbd & 
                                        "ing "));            -- Str


         -- Overwrite the word "language" with "language" + additional text.
         Document(1) :=
           ASUnb.Overwrite(Document(1),
                           ASUnb.Index(Document(1),
                                       ASUnb.To_String(
                                         ASUnb.Tail(Document(1), 8, ' ')),
                                       Ada.Strings.Backward),
                           "language designed to support long-lifed");


         -- Replace the word "lifed" with "lived".
         Document(1) :=
           ASUnb.Replace_Slice(Document(1),
                               ASUnb.Index(Document(1), "lifed"),
                               ASUnb.Length(Document(1)),
                               "lived");


         -- Overwrite the word "lived" with "lived" + additional text.
         Document(1) :=
           ASUnb.Overwrite(Document(1),
                           ASUnb.Index(Document(1),
                                       ASUnb.To_String(
                                         ASUnb.Tail(Document(1), 5, ' ')),
                                       Ada.Strings.Backward),
                               "lived, reliable software systems.");
                              
                              
         -- Use several of the overloaded versions of "&" to form this
         -- unbounded string.

         Document(2) := 'G' &                               
                        ASUnb.To_Unbounded_String("o ") & 
                        ASUnb.To_Unbounded_String("with") &
                        ' ' & 
                        "Ada!";

      end Enter_Text_Into_Document;


      -----


      procedure Create_Camera_Ready_Copy 
                  (Document    : in     Document_Type;
                   Camera_Copy :    out Camera_Ready_Copy_Type) is
      begin
         -- Break the unbounded strings into fixed lengths.

         -- Search the first unbounded string for portions of text that
         -- are less than or equal to the length of a string in the 
         -- Camera_Ready_Copy_Type object.  

         Camera_Copy(1) :=                 -- Take characters 1-39,
           ASUnb.Slice(Document(1),        -- and append a blank space.
                       1,
                       ASUnb.Index(ASUnb.To_Unbounded_String(
                                      ASUnb.Slice(Document(1),
                                                  1,
                                                  Line_Length)),
                                   Ada.Strings.Maps.To_Set(' '),
                                   Ada.Strings.Inside,
                                   Ada.Strings.Backward)) & ' ';

         Camera_Copy(2) :=                 -- Take characters 40-79.
           ASUnb.Slice(Document(1),
                       40,
                       (ASUnb.Index_Non_Blank          -- Should return 79
                          (ASUnb.To_Unbounded_String
                             (ASUnb.Slice(Document(1), -- Slice (40..79)
                                         40,
                                         79)),
                           Ada.Strings.Backward) + 39)); -- Increment since
                                                         -- this slice starts
                                                         -- at 40.

         Camera_Copy(3)(1..9) := ASUnb.Slice(Document(1), -- Characters 80-88
                                             80,
                                             ASUnb.Length(Document(1))); 
                                

         -- Break the second unbounded string into the appropriate length.
         -- It is only twelve characters in length, so the entire unbounded
         -- string will be placed on one string of the output object.

         Camera_Copy(4)(1..ASUnb.Length(Document(2))) :=
           ASUnb.To_String(ASUnb.Head(Document(2),
                                      ASUnb.Length(Document(2))));

      end Create_Camera_Ready_Copy;


      -----


      function Valid_Proofread (Draft, Master : Camera_Ready_Copy_Type) 
        return Boolean is
      begin

         -- Evaluate strings for equality, using the operators defined in
         -- package Ada.Strings.Unbounded.  The less than/greater than or
         -- equal comparisons should evaluate to "equals => True".

         if ASUnb.To_Unbounded_String(Draft(1))   =       -- "="(Unb,Unb)
            ASUnb.To_Unbounded_String(Master(1))   and    
            ASUnb.To_Unbounded_String(Draft(2))  <=       -- "<="(Unb,Unb)
            ASUnb.To_Unbounded_String(Master(2))   and
            ASUnb.To_Unbounded_String(Draft(3))  >=       -- ">="(Unb,Unb)
            ASUnb.To_Unbounded_String(Master(3))   and
            ASUnb.To_Unbounded_String(Draft(4))   =       -- "="(Unb,Unb)
            ASUnb.To_Unbounded_String(Master(4))
         then
            return True;
         else
            return False;
         end if;

      end Valid_Proofread;


      -----


   begin

      -- Enter text into the unbounded string paragraphs of the document.

      Enter_Text_Into_Document (Pamphlet);


      -- Reformat the unbounded strings into fixed string format.

      Create_Camera_Ready_Copy (Document    => Pamphlet,
                                Camera_Copy => Camera_Ready_Copy);


      -- Verify the conversion process.

      if not Valid_Proofread (Draft  => Camera_Ready_Copy,
                              Master => TC_Finished_Product)
      then
         Report.Failed ("Incorrect string processing result");
      end if;


   exception
      when others => Report.Failed ("Exception raised in Test_Block");
   end Test_Block;


   Report.Result;

end CXA4010;
