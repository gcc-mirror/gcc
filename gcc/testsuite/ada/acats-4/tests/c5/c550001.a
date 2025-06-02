-- C550001.A
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
--     Check that the loop parameter is assigned only values that satisfy the
--     predicate, in ascending order for a normal for loop and the loop body
--     is executed once for each value when the subtype has a static predicate.
--
--     Check that the loop parameter is assigned only values that satisfy the
--     predicate, in descending order for a reverse for loop and the loop body
--     is executed once for each value when the subtype has a static predicate.
--
-- TEST DESCRIPTION:
--     We try examples of each kind of discrete type. For loops are very
--     commonly used, thus we consider that any for loop is likely to occur
--     in practice; therefore we make no attempt to make the test examples
--     appear realistic.
--
--     The test reuses some type and subtype declarations from B540001, and
--     test management from C457001.
--
--     The various TC_<something> arrays contain the correct order of
--     iteration; each loop checks the actul results against those arrays.
--
-- CHANGE HISTORY:
--      22 Dec 14   RLB     Created test.
--      13 Mar 15   RLB     Eliminate overlong lines.
--
--!

with Report; use Report;
procedure C550001 is

   Named_Number : constant := 1;

   type Colors is (White, Red, Orange, Yellow, Green, Blue,
                   Indigo, Violet, Black);

   subtype Small is Integer range 0 .. 20;
   subtype Small_Zero is Integer
      with Static_Predicate => Small_Zero = 0;
   subtype Small_Even is Small
      with Static_Predicate =>
           Small_Even in 0 | 2 | 4 | 6 | 8 | 10 | 12 | 14 | 16 | 18 | 20;
   subtype Small_Odd is Small
      with Static_Predicate =>
           Small_Odd in 1 | 3 | 5 | 7 | 9 | 11 | 13 | 15 | 17 | 19;
   subtype Small_Power_of_Two is Small
      with Static_Predicate => Small_Power_of_Two in 2 | 4 | 8 | 16;
   subtype Small_Power_of_Three is Small
      with Static_Predicate => Small_Power_of_Three in 3 | 9;
   subtype Small_Null is Small_Odd range 20 .. 20;
   subtype Small_Tiny is Small_Even range 4 .. 12;

   type TC_Check_Small is array (Positive range <>) of Small'Base;
   TC_Small_Zero : constant TC_Check_Small := (1 => 0);
   TC_Small_Even : constant TC_Check_Small :=
                      (0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20);
   TC_Small_Odd  : constant TC_Check_Small :=
                      (1, 3, 5, 7, 9, 11, 13, 15, 17, 19);
   TC_Small_Pwr_2: constant TC_Check_Small := (2, 4, 8, 16);
   TC_Small_Pwr_3: constant TC_Check_Small := (3, 9);
   TC_Small_Null : constant TC_Check_Small := (1 .. 0 => 0);
   TC_Small_Tiny : constant TC_Check_Small := (4, 6, 8, 10, 12);

   subtype Total_Color is Colors
      with Static_Predicate => Total_Color in White | Black;
   subtype Primary_Color is Colors
      with Static_Predicate => Primary_Color in Red | Yellow | Blue;
   subtype Secondary_Color is Colors
      with Static_Predicate =>
           Secondary_Color in Indigo | Violet | Orange | Green;
   subtype Rainbow is Colors
      with Static_Predicate => Rainbow in Red .. Violet;

   type TC_Check_Color is array (Positive range <>) of Colors'Base;
   TC_Total_Color : constant TC_Check_Color := (White, Black);
   TC_Primary_Color : constant TC_Check_Color := (Red, Yellow, Blue);
   TC_Secondary_Color : constant TC_Check_Color :=
                                           (Orange, Green, Indigo, Violet);
   TC_Rainbow : constant TC_Check_Color := (Red, Orange, Yellow, Green, Blue,
                   Indigo, Violet);

   type Score_Base is mod 2**6;
   -- Darts scoring, borrowed from the Ada 2012 Rationale:
   subtype Single is Score_Base range 1 .. 20;
   subtype Double is Score_Base
      with Static_Predicate =>
         Double in 2 | 4 | 6 | 8 | 10 | 12 | 14 | 16 | 18 | 20 |
                   22 | 24 | 26 | 28 | 30 | 32 | 34 | 36 | 38 | 40;
   subtype Treble is Score_Base
      with Static_Predicate =>
         Treble in 3 | 6 | 9 | 12 | 15 | 18 | 21 | 24 | 27 | 30 |
                   33 | 36 | 39 | 42 | 45 | 48 | 51 | 54 | 57 | 60;
   subtype Score is Score_Base
      with Static_Predicate =>
         Score in Single | Double | Treble | 25 | 50;

   type TC_Check_Score is array (Positive range <>) of Score'Base;
   TC_Score_Single : constant TC_Check_Score := (1, 2, 3, 4, 5, 6, 7, 8, 9,
                                                10, 11, 12, 13, 14, 15, 16,
                                                17, 18, 19, 20);
   TC_Score_Double : constant TC_Check_Score := (2, 4, 6, 8, 10, 12, 14, 16,
                                                18, 20, 22, 24, 26, 28, 30, 32,
                                                34, 36, 38, 40);
   TC_Score_Treble : constant TC_Check_Score := (3, 6, 9, 12, 15, 18, 21, 24,
                                                27, 30, 33, 36, 39, 42, 45, 48,
                                                51, 54, 57, 60);
   TC_Score : constant TC_Check_Score := (1, 2, 3, 4, 5, 6, 7, 8, 9,
                                         10, 11, 12, 13, 14, 15, 16, 17, 18,
                                         19, 20, 21, 22, 24, 25, 26, 27, 28,
                                         30, 32, 33, 34, 36, 38, 39, 40, 42,
                                         45, 48, 50, 51, 54, 57, 60);

   TC_Pos_Next_Check  : Natural := Natural'First;

   procedure TC_Forward_Check_Small (X : Small'Base;
                                     Results : TC_Check_Small;
                                     Subtest : String) is
      -- Before using, initialize TC_Pos_Next_Check to Results'First.
   begin
      if TC_Pos_Next_Check not in Results'range then
         Failed ("Loop parameter has too many values:" & Small'Image(X) &
                  " (" & Subtest & ")");
      elsif Results (TC_Pos_Next_Check) /= X then
         Failed ("Loop parameter has wrong value" & Small'Image(X) &
                 "; expected" & Small'Image(Results(TC_Pos_Next_Check)) &
                  " (" & Subtest & ")");
      end if;
      TC_Pos_Next_Check := TC_Pos_Next_Check + 1;
   end TC_Forward_Check_Small;


   procedure TC_Backward_Check_Small (X : Small'Base;
                                      Results : TC_Check_Small;
                                      Subtest : String) is
      -- Before using, initialize TC_Pos_Next_Check to Results'Last.
   begin
      if TC_Pos_Next_Check not in Results'range then
         Failed ("Loop parameter has too many values:" & Small'Image(X) &
                  " (" & Subtest & ")");
      elsif Results (TC_Pos_Next_Check) /= X then
         Failed ("Loop parameter has wrong value" & Small'Image(X) &
                 "; expected" & Small'Image(Results(TC_Pos_Next_Check)) &
                  " (" & Subtest & ")");
      end if;
      TC_Pos_Next_Check := TC_Pos_Next_Check - 1;
   end TC_Backward_Check_Small;


   procedure TC_Forward_Check_Color (X : Colors'Base;
                                     Results : TC_Check_Color;
                                     Subtest : String) is
      -- Before using, initialize TC_Pos_Next_Check to Results'First.
   begin
      if TC_Pos_Next_Check not in Results'range then
         Failed ("Loop parameter has too many values:" & Colors'Image(X) &
                  " (" & Subtest & ")");
      elsif Results (TC_Pos_Next_Check) /= X then
         Failed ("Loop parameter has wrong value " & Colors'Image(X) &
                 "; expected " & Colors'Image(Results(TC_Pos_Next_Check)) &
                  " (" & Subtest & ")");
      end if;
      TC_Pos_Next_Check := TC_Pos_Next_Check + 1;
   end TC_Forward_Check_Color;


   procedure TC_Backward_Check_Color (X : Colors'Base;
                                      Results : TC_Check_Color;
                                      Subtest : String) is
      -- Before using, initialize TC_Pos_Next_Check to Results'Last.
   begin
      if TC_Pos_Next_Check not in Results'range then
         Failed ("Loop parameter has too many values:" & Colors'Image(X) &
                  " (" & Subtest & ")");
      elsif Results (TC_Pos_Next_Check) /= X then
         Failed ("Loop parameter has wrong value " & Colors'Image(X) &
                 "; expected " & Colors'Image(Results(TC_Pos_Next_Check)) &
                  " (" & Subtest & ")");
      end if;
      TC_Pos_Next_Check := TC_Pos_Next_Check - 1;
   end TC_Backward_Check_Color;


   procedure TC_Forward_Check_Score (X : Score'Base;
                                     Results : TC_Check_Score;
                                     Subtest : String) is
      -- Before using, initialize TC_Pos_Next_Check to Results'First.
   begin
      if TC_Pos_Next_Check not in Results'range then
         Failed ("Loop parameter has too many values:" & Score'Image(X) &
                  " (" & Subtest & ")");
      elsif Results (TC_Pos_Next_Check) /= X then
         Failed ("Loop parameter has wrong value" & Score'Image(X) &
                 "; expected" & Score'Image(Results(TC_Pos_Next_Check)) &
                  " (" & Subtest & ")");
      end if;
      TC_Pos_Next_Check := TC_Pos_Next_Check + 1;
   end TC_Forward_Check_Score;


   procedure TC_Backward_Check_Score (X : Score'Base;
                                      Results : TC_Check_Score;
                                      Subtest : String) is
      -- Before using, initialize TC_Pos_Next_Check to Results'Last.
   begin
      if TC_Pos_Next_Check not in Results'range then
         Failed ("Loop parameter has too many values:" & Score'Image(X) &
                  " (" & Subtest & ")");
      elsif Results (TC_Pos_Next_Check) /= X then
         Failed ("Loop parameter has wrong value" & Score'Image(X) &
                 "; expected" & Score'Image(Results(TC_Pos_Next_Check)) &
                  " (" & Subtest & ")");
      end if;
      TC_Pos_Next_Check := TC_Pos_Next_Check - 1;
   end TC_Backward_Check_Score;


   procedure TC_Check_Forward_Iters (Expected_Stop : Natural;
                                     Start   : Natural;
                                     Subtest : String) is
   begin
       if TC_Pos_Next_Check-1 /= Expected_Stop then
           Failed ("Incorrect number of iterations:" &
                   Natural'Image(TC_Pos_Next_Check-Start+1) &
                   "; expected" & Natural'Image(Expected_Stop-Start+1) &
                   " (" & Subtest & ")");
       end if;
   end TC_Check_Forward_Iters;


   procedure TC_Check_Backward_Iters (Expected_Stop : Natural;
                                      Start   : Natural;
                                      Subtest : String) is
   begin
       if TC_Pos_Next_Check+1 /= Expected_Stop then
           Failed ("Incorrect number of iterations:" &
                   Natural'Image(Start-TC_Pos_Next_Check+1) &
                   "; expected" & Natural'Image(Start-Expected_Stop+1) &
                   " (" & Subtest & ")");
       end if;
   end TC_Check_Backward_Iters;


   generic
      type GDisc is (<>);
      Subtest : String;
      type TC_Chk is array (Positive range <>) of GDisc'Base;
      TC_Chk_Data : TC_Chk;
      with procedure TC_Forward_Check (X : GDisc'Base;
                                       Results : TC_Chk;
                                       Subtest : String);
      with procedure TC_Backward_Check (X : GDisc'Base;
                                        Results : TC_Chk;
                                        Subtest : String);
   procedure Check_Disc_Iters;

   procedure Check_Disc_Iters is
   begin
       TC_Pos_Next_Check := TC_Chk_Data'First;
       for Idx in GDisc loop
           TC_Forward_Check (Idx, TC_Chk_Data, Subtest & " fwd");
       end loop;
       TC_Check_Forward_Iters (TC_Chk_Data'Last, TC_Chk_Data'First,
                               Subtest & " fwd");

       TC_Pos_Next_Check := TC_Chk_Data'Last;
       for Idx in reverse GDisc loop
           TC_Backward_Check (Idx, TC_Chk_Data, Subtest & " rev");
       end loop;
       TC_Check_Backward_Iters (TC_Chk_Data'First, TC_Chk_Data'Last,
                                Subtest & " rev");
   end Check_Disc_Iters;


   generic
      type GInt is range <>;
      Subtest : String;
      type TC_Chk is array (Positive range <>) of GInt'Base;
      TC_Chk_Data : TC_Chk;
      with procedure TC_Forward_Check (X : GInt'Base;
                                       Results : TC_Chk;
                                       Subtest : String);
      with procedure TC_Backward_Check (X : GInt'Base;
                                        Results : TC_Chk;
                                        Subtest : String);
   procedure Check_Int_Iters;

   procedure Check_Int_Iters is
   begin
       TC_Pos_Next_Check := TC_Chk_Data'First;
       for Idx in GInt loop
           TC_Forward_Check (Idx, TC_Chk_Data, Subtest & " fwd");
       end loop;
       TC_Check_Forward_Iters (TC_Chk_Data'Last, TC_Chk_Data'First,
                               Subtest & " fwd");

       TC_Pos_Next_Check := TC_Chk_Data'Last;
       for Idx in reverse GInt loop
           TC_Backward_Check (Idx, TC_Chk_Data, Subtest & " rev");
       end loop;
       TC_Check_Backward_Iters (TC_Chk_Data'First, TC_Chk_Data'Last,
                                Subtest & " rev");
   end Check_Int_Iters;


   generic
      type GMod is mod <>;
      Subtest : String;
      type TC_Chk is array (Positive range <>) of GMod'Base;
      TC_Chk_Data : TC_Chk;
      with procedure TC_Forward_Check (X : GMod'Base;
                                       Results : TC_Chk;
                                       Subtest : String);
      with procedure TC_Backward_Check (X : GMod'Base;
                                        Results : TC_Chk;
                                        Subtest : String);
   procedure Check_Mod_Iters;

   procedure Check_Mod_Iters is
   begin
       TC_Pos_Next_Check := TC_Chk_Data'First;
       for Idx in GMod loop
           TC_Forward_Check (Idx, TC_Chk_Data, Subtest & " fwd");
       end loop;
       TC_Check_Forward_Iters (TC_Chk_Data'Last, TC_Chk_Data'First,
                               Subtest & " fwd");

       TC_Pos_Next_Check := TC_Chk_Data'Last;
       for Idx in reverse GMod loop
           TC_Backward_Check (Idx, TC_Chk_Data, Subtest & " rev");
       end loop;
       TC_Check_Backward_Iters (TC_Chk_Data'First, TC_Chk_Data'Last,
                                Subtest & " rev");
   end Check_Mod_Iters;

begin
   Test
     ("C550001",
      "For a subtype with a static predicate, check that a for " &
      "loop parameter is only assigned values that satisfy the " &
      "predicates and does so in the correct order");

   TC_Pos_Next_Check := TC_Small_Zero'First;
   for Idx in Small_Zero loop
       TC_Forward_Check_Small (Idx, TC_Small_Zero, "Small_Zero fwd");
   end loop;
   TC_Check_Forward_Iters (TC_Small_Zero'Last, TC_Small_Zero'First,
                           "Small_Zero fwd");

   TC_Pos_Next_Check := TC_Small_Even'First;
   for Idx in Small_Even loop
       TC_Forward_Check_Small (Idx, TC_Small_Even, "Small_Even fwd");
   end loop;
   TC_Check_Forward_Iters (TC_Small_Even'Last, TC_Small_Even'First,
                           "Small_Even fwd");

   TC_Pos_Next_Check := TC_Small_Even'Last;
   for Idx in reverse Small_Even loop
       TC_Backward_Check_Small (Idx, TC_Small_Even, "Small_Even rev");
   end loop;
   TC_Check_Backward_Iters (TC_Small_Even'First, TC_Small_Even'Last,
                            "Small_Even rev");

   -- Try a loop exit (taken):
   TC_Pos_Next_Check := TC_Small_Odd'First;
   for Idx in Small_Odd loop
       TC_Forward_Check_Small (Idx, TC_Small_Odd, "Small_Odd fwd");
       exit when Idx = TC_Small_Odd(7);
   end loop;
   TC_Check_Forward_Iters (7, TC_Small_Odd'First,
                           "Small_Odd fwd");

   TC_Pos_Next_Check := TC_Small_Odd'Last;
   for Idx in reverse Small_Odd loop
       TC_Backward_Check_Small (Idx, TC_Small_Odd, "Small_Odd rev");
       exit when Idx = TC_Small_Odd(6);
   end loop;
   TC_Check_Backward_Iters (6, TC_Small_Odd'Last,
                            "Small_Odd rev");

   -- Try a loop exit (not taken):
   TC_Pos_Next_Check := TC_Small_Pwr_2'First;
   for Idx in Small_Power_of_Two loop
       TC_Forward_Check_Small (Idx, TC_Small_Pwr_2, "Small_Pwr_2 fwd");
       exit when Idx = 12;
   end loop;
   TC_Check_Forward_Iters (TC_Small_Pwr_2'Last, TC_Small_Pwr_2'First,
                           "Small_Pwr_2 fwd");

   TC_Pos_Next_Check := TC_Small_Pwr_3'Last;
   for Idx in reverse Small_Power_of_Three loop
       TC_Backward_Check_Small (Idx, TC_Small_Pwr_3, "Small_Pwr_3 rev");
       exit when Idx = 27;
   end loop;
   TC_Check_Backward_Iters (TC_Small_Pwr_3'First, TC_Small_Pwr_3'Last,
                            "Small_Pwr_3 rev");

   -- Null loop:
   TC_Pos_Next_Check := TC_Small_Null'First;
   for Idx in Small_Null loop
       TC_Forward_Check_Small (Idx, TC_Small_Null, "Small_Null fwd");
   end loop;
   TC_Check_Forward_Iters (TC_Small_Null'Last, TC_Small_Null'First,
                            "Small_Null fwd");

   TC_Pos_Next_Check := TC_Small_Null'Last;
   for Idx in reverse Small_Null loop
       TC_Backward_Check_Small (Idx, TC_Small_Null, "Small_Null rev");
   end loop;
   TC_Check_Backward_Iters (TC_Small_Null'First, TC_Small_Null'Last,
                            "Small_Null rev");

   TC_Pos_Next_Check := TC_Small_Tiny'First;
   for Idx in Small_Tiny loop
       TC_Forward_Check_Small (Idx, TC_Small_Tiny, "Small_Tiny fwd");
   end loop;
   TC_Check_Forward_Iters (TC_Small_Tiny'Last, TC_Small_Tiny'First,
                            "Small_Tiny fwd");

   TC_Pos_Next_Check := TC_Small_Tiny'Last;
   for Idx in reverse Small_Tiny loop
       TC_Backward_Check_Small (Idx, TC_Small_Tiny, "Small_Tiny rev");
   end loop;
   TC_Check_Backward_Iters (TC_Small_Tiny'First, TC_Small_Tiny'Last,
                            "Small_Tiny rev");

   -- Enumeration cases:

   TC_Pos_Next_Check := TC_Primary_Color'First;
   for Idx in Primary_Color loop
       TC_Forward_Check_Color (Idx, TC_Primary_Color, "Primary_Color fwd");
   end loop;
   TC_Check_Forward_Iters (TC_Primary_Color'Last, TC_Primary_Color'First,
                            "Primary_Color fwd");

   TC_Pos_Next_Check := TC_Primary_Color'Last;
   for Idx in reverse Primary_Color loop
       TC_Backward_Check_Color (Idx, TC_Primary_Color, "Primary_Color rev");
   end loop;
   TC_Check_Backward_Iters (TC_Primary_Color'First, TC_Primary_Color'Last,
                            "Primary_Color rev");

   -- Iteration order different than declaration:
   TC_Pos_Next_Check := TC_Secondary_Color'First;
   for Idx in Secondary_Color loop
       TC_Forward_Check_Color (Idx, TC_Secondary_Color, "Secondary_Color fwd");
   end loop;
   TC_Check_Forward_Iters (TC_Secondary_Color'Last, TC_Secondary_Color'First,
                            "Secondary_Color fwd");

   TC_Pos_Next_Check := TC_Secondary_Color'Last;
   for Idx in reverse Secondary_Color loop
       TC_Backward_Check_Color (Idx, TC_Secondary_Color,
                                "Secondary_Color rev");
   end loop;
   TC_Check_Backward_Iters (TC_Secondary_Color'First, TC_Secondary_Color'Last,
                            "Secondary_Color rev");

   -- Try a loop exit (taken):
   TC_Pos_Next_Check := TC_Rainbow'First;
   for Idx in Rainbow loop
       TC_Forward_Check_Color (Idx, TC_Rainbow, "Rainbow fwd");
       exit when Idx = Blue;
   end loop;
   TC_Check_Forward_Iters (5, TC_Rainbow'First, "Rainbow fwd");

   TC_Pos_Next_Check := TC_Rainbow'Last;
   for Idx in reverse Rainbow loop
       TC_Backward_Check_Color (Idx, TC_Rainbow, "Rainbow rev");
       exit when Idx = Blue;
   end loop;
   TC_Check_Backward_Iters (5, TC_Rainbow'Last, "Rainbow rev");

   -- Try a loop exit (not taken):
   TC_Pos_Next_Check := TC_Total_Color'First;
   for Idx in Total_Color loop
       TC_Forward_Check_Color (Idx, TC_Total_Color, "Total_Color fwd");
       exit when Idx = Blue;
   end loop;
   TC_Check_Forward_Iters (TC_Total_Color'Last, TC_Total_Color'First,
                            "Total_Color fwd");

   TC_Pos_Next_Check := TC_Total_Color'Last;
   for Idx in reverse Total_Color loop
       TC_Backward_Check_Color (Idx, TC_Total_Color, "Total_Color rev");
       exit when Idx = Blue;
   end loop;
   TC_Check_Backward_Iters (TC_Total_Color'First, TC_Total_Color'Last,
                            "Total_Color rev");

   -- Modular cases:

   TC_Pos_Next_Check := TC_Score_Double'First;
   for Idx in Double loop
       TC_Forward_Check_Score (Idx, TC_Score_Double, "Double fwd");
   end loop;
   TC_Check_Forward_Iters (TC_Score_Double'Last, TC_Score_Double'First,
                            "Double fwd");

   TC_Pos_Next_Check := TC_Score_Double'Last;
   for Idx in reverse Double loop
       TC_Backward_Check_Score (Idx, TC_Score_Double, "Double rev");
   end loop;
   TC_Check_Backward_Iters (TC_Score_Double'First, TC_Score_Double'Last,
                            "Double rev");

   -- Iteration order different than declaration:
   TC_Pos_Next_Check := TC_Score'First;
   for Idx in Score loop
       TC_Forward_Check_Score (Idx, TC_Score, "Score fwd");
   end loop;
   TC_Check_Forward_Iters (TC_Score'Last, TC_Score'First,
                            "Score fwd");

   TC_Pos_Next_Check := TC_Score'Last;
   for Idx in reverse Score loop
       TC_Backward_Check_Score (Idx, TC_Score, "Score rev");
   end loop;
   TC_Check_Backward_Iters (TC_Score'First, TC_Score'Last,
                            "Score fwd");

   -- Try a loop exit (taken):
   TC_Pos_Next_Check := TC_Score_Single'First;
   for Idx in Single loop
       TC_Forward_Check_Score (Idx, TC_Score_Single, "Single fwd");
       exit when Idx = 10;
   end loop;
   TC_Check_Forward_Iters (10, TC_Score_Single'Last,
                            "Single fwd");

   TC_Pos_Next_Check := TC_Score_Single'Last;
   for Idx in reverse Single loop
       TC_Backward_Check_Score (Idx, TC_Score_Single, "Single rev");
       exit when Idx = 7;
   end loop;
   TC_Check_Backward_Iters (7, TC_Score_Single'Last,
                            "Single rev");

   -- Try a loop exit (not taken):
   TC_Pos_Next_Check := TC_Score_Treble'First;
   for Idx in Treble loop
       TC_Forward_Check_Score (Idx, TC_Score_Treble, "Treble fwd");
       exit when Idx = 25;
   end loop;
   TC_Check_Forward_Iters (TC_Score_Treble'Last, TC_Score_Treble'First,
                           "Treble fwd");

   TC_Pos_Next_Check := TC_Score_Treble'Last;
   for Idx in reverse Treble loop
       TC_Backward_Check_Score (Idx, TC_Score_Treble, "Treble rev");
       exit when Idx = 25;
   end loop;
   TC_Check_Backward_Iters (TC_Score_Treble'First, TC_Score_Treble'Last,
                            "Treble rev");

   declare
      procedure Check_Odd is new Check_Int_Iters
         (Small_Odd, "Gen-Int Small_Odd",
          TC_Check_Small, TC_Small_Odd,
          TC_Forward_Check_Small, TC_Backward_Check_Small);

      procedure Check_Treble is new Check_Mod_Iters
         (Treble, "Gen-Int Treble",
          TC_Check_Score, TC_Score_Treble,
          TC_Forward_Check_Score, TC_Backward_Check_Score);

      procedure Check_Pwr_2 is new Check_Disc_Iters
         (Small_Power_of_Two, "Gen-Disc Small_Pwr_2",
          TC_Check_Small, TC_Small_Pwr_2,
          TC_Forward_Check_Small, TC_Backward_Check_Small);

      procedure Check_Sec_Color is new Check_Disc_Iters
         (Secondary_Color, "Gen-Disc Secondary_Color",
          TC_Check_Color, TC_Secondary_Color,
          TC_Forward_Check_Color, TC_Backward_Check_Color);

      procedure Check_Score is new Check_Disc_Iters
         (Score, "Gen-Disc Score",
          TC_Check_Score, TC_Score,
          TC_Forward_Check_Score, TC_Backward_Check_Score);

   begin
      Check_Odd;
      Check_Treble;
      Check_Pwr_2;
      Check_Sec_Color;
      Check_Score;
   end;

   Result;

end C550001;
