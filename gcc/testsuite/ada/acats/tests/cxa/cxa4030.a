-- CXA4030.A
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
--      Check that Ada.Strings.Unbounded versions of subprograms Translate 
--      (procedure and function), Index, and Count, which use a 
--      Maps.Character_Mapping_Function input parameter, produce correct 
--      results.
--
-- TEST DESCRIPTION:
--      This test examines the operation of the four subprograms contained
--      in the Ada.Strings.Unbounded package that use a
--      Character_Mapping_Function parameter to provide the mapping 
--      capability.
--      Two Character_Mapping_Function objects are defined that reference
--      subprograms contained in the Ada.Characters.Handling package;
--      To_Lower will return the lower-case form of the character provided
--      as the input parameter, To_Upper will return the upper-case form
--      of the character input parameter (provided there is an upper-case
--      form).
--      In several instances in this test, the character handling functions 
--      are referenced directly in the parameter list of the subprograms
--      under test, demonstrating another form of expected common usage.
--      
--      Results of all subprograms are compared with expected results.
--      
--      This test, when taken in conjunction with tests CXA4010, CXA4011,
--      CXA4031, and CXA4032 will constitute a test of all the functionality 
--      contained in package Ada.Strings.Unbounded.  This test uses a variety 
--      of the subprograms defined in the unbounded string package in ways 
--      typical of common usage.
--      
--       
-- CHANGE HISTORY:
--      21 Feb 95   SAIC    Initial prerelease version
--      21 Apr 95   SAIC    Modified header commentary.
--
--!

with Ada.Strings.Unbounded;
with Ada.Strings.Maps;
with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Report;

procedure CXA4030 is

begin

   Report.Test ("CXA4030", "Check that Ada.Strings.Unbounded versions " &
                           "of subprograms Translate (procedure and "   &
                           "function), Index, and Count, which use a "  &
                           "Maps.Character_Mapping_Function input "     &
                           "parameter, produce correct results");

   Test_Block:
   declare

      package Unb renames Ada.Strings.Unbounded;
      use type Unb.Unbounded_String;
      use Ada.Strings;
      use Ada.Characters;


      -- The following strings are used in examination of the Translation
      -- subprograms.

      New_Character_String : Unb.Unbounded_String :=
                               Unb.To_Unbounded_String(
                                 Latin_1.LC_A_Grave          &
                                 Latin_1.LC_A_Ring           &
                                 Latin_1.LC_AE_Diphthong     &
                                 Latin_1.LC_C_Cedilla        &
                                 Latin_1.LC_E_Acute          &
                                 Latin_1.LC_I_Circumflex     &
                                 Latin_1.LC_Icelandic_Eth    &
                                 Latin_1.LC_N_Tilde          &
                                 Latin_1.LC_O_Oblique_Stroke &
                                 Latin_1.LC_Icelandic_Thorn);  
 

      TC_New_Character_String : Unb.Unbounded_String :=
                                  Unb.To_Unbounded_String(
                                    Latin_1.UC_A_Grave          &
                                    Latin_1.UC_A_Ring           &
                                    Latin_1.UC_AE_Diphthong     &
                                    Latin_1.UC_C_Cedilla        &
                                    Latin_1.UC_E_Acute          &
                                    Latin_1.UC_I_Circumflex     &
                                    Latin_1.UC_Icelandic_Eth    &
                                    Latin_1.UC_N_Tilde          &
                                    Latin_1.UC_O_Oblique_Stroke &
                                    Latin_1.UC_Icelandic_Thorn);  
                     

      -- In this test, access objects are defined to refer to two functions
      -- from the Ada.Characters.Handling package.  These access objects 
      -- will be provided as parameters to the subprograms under test.
      -- Note: There will be several examples in this test of these character 
      --       handling functions being referenced directly within the 
      --       parameter list of the subprograms under test.

      Map_To_Lower_Case_Ptr : Maps.Character_Mapping_Function :=
                                Handling.To_Lower'Access;

      Map_To_Upper_Case_Ptr : Maps.Character_Mapping_Function :=
                                Handling.To_Upper'Access;

   begin

      -- Function Index, Forward direction search.
      -- Note: Several of the following cases use the default value
      --       Forward for the Going parameter.

      if Unb.Index(Source => Unb.To_Unbounded_String(
                               "The library package Strings.Unbounded"),
                   Pattern => "unb",
                   Going   => Ada.Strings.Forward,
                   Mapping => Map_To_Lower_Case_Ptr)    /= 29   or

         Unb.Index(Unb.To_Unbounded_String(
                     "THE RAIN IN SPAIN FALLS MAINLY ON THE PLAIN"),
                   "ain",
                   Mapping => Map_To_Lower_Case_Ptr)    /= 6    or

         Unb.Index(Unb.To_Unbounded_String("maximum number"),
                   "um",
                   Ada.Strings.Forward,
                   Handling.To_Lower'Access)            /= 6    or

         Unb.Index(Unb.To_Unbounded_String("CoMpLeTeLy MiXeD CaSe StRiNg"),
                   "MIXED CASE STRING",
                   Ada.Strings.Forward,
                   Map_To_Upper_Case_Ptr)               /= 12   or

         Unb.Index(Unb.To_Unbounded_String(
                     "STRING WITH NO MATCHING PATTERNS"),
                   "WITH",
                   Mapping => Map_To_Lower_Case_Ptr)    /= 0    or

         Unb.Index(Unb.To_Unbounded_String("THIS STRING IS IN UPPER CASE"),
                   "IS",
                   Ada.Strings.Forward,
                   Handling.To_Upper'Access)            /= 3    or

         Unb.Index(Unb.Null_Unbounded_String,
                   "is",
                   Mapping => Map_To_Lower_Case_Ptr)    /= 0    or

         Unb.Index(Unb.To_Unbounded_String("AAABBBaaabbb"),
                   "aabb",
                   Mapping => Handling.To_Lower'Access) /= 2
      then
         Report.Failed("Incorrect results from Function Index, going "    &
                       "in Forward direction, using a Character Mapping " &
                       "Function parameter");
      end if;



      -- Function Index, Backward direction search.

      if Unb.Index(Unb.To_Unbounded_String("Case of a Mixed Case String"), 
                   "case", 
                   Ada.Strings.Backward,
                   Map_To_Lower_Case_Ptr)               /= 17   or

         Unb.Index(Unb.To_Unbounded_String("Case of a Mixed Case String"), 
                   "CASE", 
                   Ada.Strings.Backward,
                   Mapping => Map_To_Upper_Case_Ptr)    /= 17   or

         Unb.Index(Unb.To_Unbounded_String("rain, Rain, and more RAIN"),
                   "rain",
                   Ada.Strings.Backward,
                   Handling.To_Lower'Access)            /= 22   or

         Unb.Index(Unb.To_Unbounded_String("RIGHT place, right time"),
                   "RIGHT",
                   Ada.Strings.Backward,
                   Handling.To_Upper'Access)            /= 14   or

         Unb.Index(Unb.To_Unbounded_String("WOULD MATCH BUT FOR THE CASE"),
                   "WOULD MATCH BUT FOR THE CASE",
                   Going   => Ada.Strings.Backward,
                   Mapping => Map_To_Lower_Case_Ptr)    /= 0
      then
         Report.Failed("Incorrect results from Function Index, going "     &
                       "in Backward direction, using a Character Mapping " &
                       "Function parameter");
      end if;



      -- Function Index, Pattern_Error if Pattern = Null_String

      declare
         use Unbounded;
         Null_String : constant String := "";
         TC_Natural  : Natural         := 1000;
      begin
         TC_Natural := Index(To_Unbounded_String("A Valid Unbounded String"), 
                             Null_String,
                             Going   => Ada.Strings.Forward,
                             Mapping => Handling.To_Lower'Access);
         Report.Failed("Pattern_Error not raised by Function Index when " &
                       "given a null pattern string");
      exception
         when Pattern_Error => null;   -- OK, expected exception.
         when others        =>
            Report.Failed("Incorrect exception raised by Function Index " &
                          "using a Character Mapping Function parameter " &
                          "when given a null pattern string");
      end;



      -- Function Count.

      if Unb.Count(Source  => Unb.To_Unbounded_String("ABABABA"),       
                   Pattern => "aba",
                   Mapping => Map_To_Lower_Case_Ptr)   /=  2   or

         Unb.Count(Unb.To_Unbounded_String("ABABABA"), 
                   "ABA", 
                   Mapping => Map_To_Lower_Case_Ptr)   /=  0   or

         Unb.Count(Unb.To_Unbounded_String("This IS a MISmatched issue"),
                   "is",
                   Handling.To_Lower'Access)           /=  4   or

         Unb.Count(Unb.To_Unbounded_String("ABABABA"), 
                   "ABA", 
                   Map_To_Upper_Case_Ptr)              /=  2   or

         Unb.Count(Unb.To_Unbounded_String("This IS a MISmatched issue"),
                   "is",
                   Mapping => Map_To_Upper_Case_Ptr)   /=  0   or

         Unb.Count(Unb.To_Unbounded_String(
                     "She sells sea shells by the sea shore"),
                   "s",
                   Handling.To_Lower'Access)           /=  8   or

         Unb.Count(Unb.Null_Unbounded_String,      
                   "match",
                   Map_To_Upper_Case_Ptr)              /=  0
      then
         Report.Failed("Incorrect results from Function Count, using " &
                       "a Character Mapping Function parameter");
      end if;



      -- Function Count, Pattern_Error if Pattern = Null_String

      declare
         use Ada.Strings.Unbounded;
         Null_Pattern_String : constant String := "";
         TC_Natural          : Natural         := 1000;
      begin
         TC_Natural := Count(To_Unbounded_String("A Valid String"), 
                             Null_Pattern_String,
                             Map_To_Lower_Case_Ptr);
         Report.Failed("Pattern_Error not raised by Function Count using " &
                       "a Character Mapping Function parameter when "      &
                       "given a null pattern string");
      exception
         when Pattern_Error => null;   -- OK, expected exception.
         when others        =>
            Report.Failed("Incorrect exception raised by Function Count " &
                          "using a Character Mapping Function parameter " &
                          "when given a null pattern string");
      end;



      -- Function Translate.

      if Unb.Translate(Source  => Unb.To_Unbounded_String(
                                    "A Sample Mixed Case String"),
                       Mapping => Map_To_Lower_Case_Ptr)           /= 
         Unb.To_Unbounded_String("a sample mixed case string")       or

         Unb.Translate(Unb.To_Unbounded_String("ALL LOWER CASE"),
                       Handling.To_Lower'Access)                   /= 
         Unb.To_Unbounded_String("all lower case")                   or

         Unb.Translate(Unb.To_Unbounded_String("end with lower case"),
                       Map_To_Lower_Case_Ptr)                      /= 
         Unb.To_Unbounded_String("end with lower case")              or

         Unb.Translate(Unb.Null_Unbounded_String,
                       Handling.To_Lower'Access)                   /=
         Unb.Null_Unbounded_String                                   or

         Unb.Translate(Unb.To_Unbounded_String("start with lower case"),
                       Map_To_Upper_Case_Ptr)                      /= 
         Unb.To_Unbounded_String("START WITH LOWER CASE")            or

         Unb.Translate(Unb.To_Unbounded_String("ALL UPPER CASE STRING"),
                       Handling.To_Upper'Access)                   /=
         Unb.To_Unbounded_String("ALL UPPER CASE STRING")            or

         Unb.Translate(Unb.To_Unbounded_String(
                         "LoTs Of MiXeD CaSe ChArAcTeRs"),
                       Map_To_Upper_Case_Ptr)                      /=
         Unb.To_Unbounded_String("LOTS OF MIXED CASE CHARACTERS")    or

         Unb.Translate(New_Character_String, 
                       Handling.To_Upper'Access)                   /=
         TC_New_Character_String

      then
         Report.Failed("Incorrect results from Function Translate, using " &
                       "a Character Mapping Function parameter");
      end if;



      -- Procedure Translate.

      declare

         use Ada.Strings.Unbounded;
         use Ada.Characters.Handling;

         Str_1    : Unbounded_String := 
                      To_Unbounded_String("AN ALL UPPER CASE STRING");
         Str_2    : Unbounded_String := 
                      To_Unbounded_String("A Mixed Case String");
         Str_3    : Unbounded_String := 
                      To_Unbounded_String("a string with lower case letters");
         TC_Str_1 : constant Unbounded_String := Str_1;
         TC_Str_3 : constant Unbounded_String := Str_3;

      begin

         Translate(Source => Str_1, Mapping => Map_To_Lower_Case_Ptr);

         if Str_1 /= To_Unbounded_String("an all upper case string") then
            Report.Failed("Incorrect result from Procedure Translate - 1");
         end if;

         Translate(Source => Str_1, Mapping => Map_To_Upper_Case_Ptr);

         if Str_1 /= TC_Str_1 then
            Report.Failed("Incorrect result from Procedure Translate - 2");
         end if;

         Translate(Str_2, Mapping => Map_To_Lower_Case_Ptr);

         if Str_2 /= To_Unbounded_String("a mixed case string") then
            Report.Failed("Incorrect result from Procedure Translate - 3");
         end if;

         Translate(Str_2, Mapping => To_Upper'Access);

         if Str_2 /= To_Unbounded_String("A MIXED CASE STRING") then
            Report.Failed("Incorrect result from Procedure Translate - 4");
         end if;

         Translate(Str_3, To_Lower'Access);

         if Str_3 /= TC_Str_3 then
            Report.Failed("Incorrect result from Procedure Translate - 5");
         end if;

         Translate(Str_3, To_Upper'Access);

         if Str_3 /= 
            To_Unbounded_String("A STRING WITH LOWER CASE LETTERS")
         then
            Report.Failed("Incorrect result from Procedure Translate - 6");
         end if;

         Translate(New_Character_String, Map_To_Upper_Case_Ptr);

         if New_Character_String /= TC_New_Character_String then
            Report.Failed("Incorrect result from Procedure Translate - 6");
         end if;

      end;


   exception
      when others => Report.Failed ("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

end CXA4030;
