-- CXA4013.A
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
--      Check that the subprograms defined in package Ada.Strings.Wide_Fixed
--      are available, and that they produce correct results.  Specifically,
--      check the subprograms Index, "*" (Wide_String constructor function),
--      Count, Trim, and Replace_Slice.
--
-- TEST DESCRIPTION:
--      This test demonstrates how certain Wide_Fixed string functions
--      are used to eliminate specific substrings from portions of text.  
--      A procedure is defined that will take as parameters a source 
--      Wide_String along with a substring that is to be completely removed
--      from the source string.  The source Wide_String is parsed using the
--      Index function, and any substring slices are replaced in the source
--      Wide_String by a series of X's (based on the length of the substring.)  
--      Three lines of text are provided to this procedure, and the resulting
--      substitutions are compared with expected results to validate the 
--      string processing.
--      A global accumulator is updated with the number of occurrences of the
--      substring in the source string.  
--
--       
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

with Ada.Strings;
with Ada.Strings.Wide_Fixed;
with Ada.Strings.Wide_Maps;
with Report;

procedure CXA4013 is

begin

   Report.Test ("CXA4013", "Check that the subprograms defined in package "  &
                           "Ada.Strings.Wide_Fixed are available, and that " &
                           "they produce correct results");

   Test_Block:
   declare

      TC_Total        : Natural  := 0;
      Number_Of_Lines : constant := 3;
      WC              : Wide_Character := 
                          Wide_Character'Val(Character'Pos('X')            + 
                                             Character'Pos(Character'Last) + 
                                             1 );

      subtype WS is Wide_String (1..25);

      type Restricted_Words_Array_Type is 
        array (1..10) of Wide_String (1..10);

      Restricted_Words : Restricted_Words_Array_Type := 
                           ("   platoon", " marines  ", "  Marines ",
                            "north     ", "south     ", "      east", 
                            "  beach   ", "   airport", "airfield  ", 
                            "     road ");

      type Page_Of_Text_Type is array (1..Number_Of_Lines) of WS;

      Text_Page : Page_Of_Text_Type := ("The platoon of Marines   ",
                                        "moved south on the south ",
                                        "road to the airfield.    ");

      TC_Revised_Line_1 : constant Wide_String := "The XXXXXXX of XXXXXXX   ";
      TC_Revised_Line_2 : constant Wide_String := "moved XXXXX on the XXXXX ";
      TC_Revised_Line_3 : constant Wide_String := "XXXX to the XXXXXXXX.    ";


      function Equivalent (Left : WS;  Right : Wide_String) 
        return Boolean is
      begin
         for i in WS'range loop
            if Left(i) /= Right(i) then
               if Left(i) /= WC   or  Right(i) /= 'X' then
                  return False;
               end if;
            end if;
         end loop;
         return True;
      end Equivalent;

      ---

      procedure Censor (Source_String  : in out Wide_String;
                        Pattern_String : in     Wide_String) is
                        
         use Ada.Strings.Wide_Fixed; -- allows infix notation of "*" below.

         -- Create a replacement string that is the same length as the
         -- pattern string being removed.  Use the infix notation of the 
         -- wide string constructor function.

         Replacement : constant Wide_String :=                       
                         Pattern_String'Length * WC;                 -- "*"

         Going     : Ada.Strings.Direction := Ada.Strings.Forward;   
         Start_Pos,
         Index     : Natural := Source_String'First;

      begin  -- Censor

         -- Accumulate count of total replacement operations.

         TC_Total := TC_Total +                                     
                      Ada.Strings.Wide_Fixed.Count                  -- Count
                        (Source  => Source_String,
                         Pattern => Pattern_String,
                         Mapping => Ada.Strings.Wide_Maps.Identity);
         loop

            Index := Ada.Strings.Wide_Fixed.Index                   -- Index
                       (Source_String(Start_Pos..Source_String'Last),
                        Pattern_String, 
                        Going, 
                        Ada.Strings.Wide_Maps.Identity);

            exit when Index = 0;   -- No matches, exit loop.

            -- if a match was found, modify the substring.
            Ada.Strings.Wide_Fixed.Replace_Slice              -- Replace_Slice
                                     (Source_String,
                                      Index,
                                      Index + Pattern_String'Length - 1,
                                      Replacement);
            Start_Pos := Index + Pattern_String'Length;

         end loop;

      end Censor;


   begin

      -- Invoke Censor subprogram to cleanse text.
      -- Loop through each line of text, and check for the presence of each
      -- restricted word.
      -- Use the Trim function to eliminate leading or trailing blanks from
      -- the restricted word parameters.

      for Line in 1..Number_Of_Lines loop
         for Word in Restricted_Words'Range loop
             Censor (Text_Page(Line),                                 -- Trim
                     Ada.Strings.Wide_Fixed.Trim(Restricted_Words(Word),
                                                 Ada.Strings.Both));
         end loop;
      end loop;


      -- Validate results.

      if TC_Total /= 6 then
         Report.Failed ("Incorrect number of substitutions performed");
      end if;

      if not Equivalent (Text_Page(1), TC_Revised_Line_1) then
         Report.Failed ("Incorrect substitutions on Line 1");
      end if;

      if not Equivalent (Text_Page(2), TC_Revised_Line_2) then
         Report.Failed ("Incorrect substitutions on Line 2");
      end if;

      if not Equivalent (Text_Page(3), TC_Revised_Line_3) then
         Report.Failed ("Incorrect substitutions on Line 3");
      end if;


   exception
      when others => Report.Failed ("Exception raised in Test_Block");
   end Test_Block;


   Report.Result;

end CXA4013;
