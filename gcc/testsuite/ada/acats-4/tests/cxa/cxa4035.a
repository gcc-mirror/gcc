-- CXA4035.A
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
-- OBJECTIVE:
--      Check that the case insensitive string comparison functions provide
--      correct results.
--
-- TEST DESCRIPTION:
--      This test checks the output of the Equal_Case_Insensitive and
--      Less_Case_Insensitive functions for each of Strings, Bounded_Strings and
--      Unbounded_Strings.
--
-- CHANGE HISTORY:
--      23 May 13   JAC     Initial pre-release version.
--      10 Jul 13   JAC     Second pre-release version.
--      21 Mar 14   RLB     Readied to issue; renamed.
--!
with Ada.Strings.Bounded;
with Ada.Strings.Bounded.Equal_Case_Insensitive;
with Ada.Strings.Bounded.Less_Case_Insensitive;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Fixed.Equal_Case_Insensitive;
with Ada.Strings.Fixed.Less_Case_Insensitive;
with Ada.Strings.Less_Case_Insensitive;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Equal_Case_Insensitive;
with Ada.Strings.Unbounded.Less_Case_Insensitive;
with Report;

procedure CXA4035 is

   -- Strings

   -- Lower case

   LC_String_1 : constant String := "cow";
   LC_String_2 : constant String := "pig";
   LC_String_3 : constant String := "cows";

   -- Title case

   TC_String_1 : constant String := "Cow";
   TC_String_2 : constant String := "Pig";
   TC_String_3 : constant String := "Cows";

   -- Upper case

   UC_String_1 : constant String := "COW";
   UC_String_2 : constant String := "PIG";
   UC_String_3 : constant String := "COWS";


   -- Bounded Strings

   package Bounded_6 is
     new Ada.Strings.Bounded.Generic_Bounded_Length (6);

   -- Lower case

   LC_Bounded_String_1 : constant Bounded_6.Bounded_String :=
     Bounded_6.To_Bounded_String ("cow");
   LC_Bounded_String_2 : constant Bounded_6.Bounded_String :=
     Bounded_6.To_Bounded_String ("pig");
   LC_Bounded_String_3 : constant Bounded_6.Bounded_String :=
     Bounded_6.To_Bounded_String ("cows");

   -- Title case

   TC_Bounded_String_1 : constant Bounded_6.Bounded_String :=
     Bounded_6.To_Bounded_String ("Cow"); -- Title case
   TC_Bounded_String_2 : constant Bounded_6.Bounded_String :=
     Bounded_6.To_Bounded_String ("Pig"); -- Title case
   TC_Bounded_String_3 : constant Bounded_6.Bounded_String :=
     Bounded_6.To_Bounded_String ("Cows"); -- Title case

   -- Upper case

   UC_Bounded_String_1 : constant Bounded_6.Bounded_String :=
     Bounded_6.To_Bounded_String ("COW");
   UC_Bounded_String_2 : constant Bounded_6.Bounded_String :=
     Bounded_6.To_Bounded_String ("PIG");
   UC_Bounded_String_3 : constant Bounded_6.Bounded_String :=
     Bounded_6.To_Bounded_String ("COWS");

   function Bounded_6_Equal_Case_Insensitive is
     new Ada.Strings.Bounded.Equal_Case_Insensitive (Bounded_6);

   function Bounded_6_Less_Case_Insensitive is
     new Ada.Strings.Bounded.Less_Case_Insensitive (Bounded_6);


   -- Unbounded Strings

   -- Lower case

   LC_Unbounded_String_1 : constant Ada.Strings.Unbounded.Unbounded_String :=
     Ada.Strings.Unbounded.To_Unbounded_String ("cow");
   LC_Unbounded_String_2 : constant Ada.Strings.Unbounded.Unbounded_String :=
     Ada.Strings.Unbounded.To_Unbounded_String ("pig");
   LC_Unbounded_String_3 : constant Ada.Strings.Unbounded.Unbounded_String :=
     Ada.Strings.Unbounded.To_Unbounded_String ("cows");

   -- Title case

   TC_Unbounded_String_1 : constant Ada.Strings.Unbounded.Unbounded_String :=
     Ada.Strings.Unbounded.To_Unbounded_String ("Cow"); -- Title case
   TC_Unbounded_String_2 : constant Ada.Strings.Unbounded.Unbounded_String :=
     Ada.Strings.Unbounded.To_Unbounded_String ("Pig"); -- Title case
   TC_Unbounded_String_3 : constant Ada.Strings.Unbounded.Unbounded_String :=
     Ada.Strings.Unbounded.To_Unbounded_String ("Cows"); -- Title case

   -- Upper case

   UC_Unbounded_String_1 : constant Ada.Strings.Unbounded.Unbounded_String :=
     Ada.Strings.Unbounded.To_Unbounded_String ("COW");
   UC_Unbounded_String_2 : constant Ada.Strings.Unbounded.Unbounded_String :=
     Ada.Strings.Unbounded.To_Unbounded_String ("PIG");
   UC_Unbounded_String_3 : constant Ada.Strings.Unbounded.Unbounded_String :=
     Ada.Strings.Unbounded.To_Unbounded_String ("COWS");

begin

   Report.Test
     ("CXA4035",
      "Check that the Equal_Case_Insensitive and Less_Case_Insensitive " &
      "functions for each of Strings, Bounded_Strings and Unbounded_Strings " &
      "provide correct results");


   -- Equal_Case_Insensitive

   -- Strings

   -- Check matching strings of same length

   if not Ada.Strings.Equal_Case_Insensitive
     (LC_String_1, TC_String_1) then

      Report.Failed
        ("Incorrect operation of function Equal_Case_Insensitive LC" &
         " with TC - matching");

   end if;

   if not Ada.Strings.Equal_Case_Insensitive
     (LC_String_1, UC_String_1) then

      Report.Failed
        ("Incorrect operation of function Equal_Case_Insensitive LC" &
         " with UC - matching");

   end if;

   if not Ada.Strings.Equal_Case_Insensitive
     (TC_String_1, UC_String_1) then

      Report.Failed
        ("Incorrect operation of function Equal_Case_Insensitive TC" &
         " with UC - matching");

   end if;

   -- Check non-matching strings of same length

   if Ada.Strings.Equal_Case_Insensitive
     (LC_String_1, LC_String_2) then

      Report.Failed
        ("Incorrect operation of function Equal_Case_Insensitive LC" &
         " - non-matching same length");

   end if;

   if Ada.Strings.Equal_Case_Insensitive
     (TC_String_1, TC_String_2) then

      Report.Failed
        ("Incorrect operation of function Equal_Case_Insensitive TC" &
         " - non-matching same length");

   end if;

   if Ada.Strings.Equal_Case_Insensitive
     (UC_String_1, UC_String_2) then

      Report.Failed
        ("Incorrect operation of function Equal_Case_Insensitive UC" &
         " - non-matching same length");

   end if;

   -- Check part-matching strings of different lengths

   if Ada.Strings.Equal_Case_Insensitive
     (LC_String_1, LC_String_3) then

      Report.Failed
        ("Incorrect operation of function Equal_Case_Insensitive LC" &
         " with longer LC - part-matching different lengths");

   end if;

   if Ada.Strings.Equal_Case_Insensitive
     (TC_String_1, TC_String_3) then

      Report.Failed
        ("Incorrect operation of function Equal_Case_Insensitive TC" &
         " with longer TC - part-matching different lengths");

   end if;

   if Ada.Strings.Equal_Case_Insensitive
     (UC_String_1, UC_String_3) then

      Report.Failed
        ("Incorrect operation of function Equal_Case_Insensitive UC" &
         " with longer UC - part-matching different lengths");

   end if;


   -- Fixed Strings

   -- Check matching strings of same length

   if not Ada.Strings.Fixed.Equal_Case_Insensitive
     (LC_String_1, TC_String_1) then

      Report.Failed
        ("Incorrect operation of function Fixed.Equal_Case_Insensitive LC" &
         " with TC - matching");

   end if;

   if not Ada.Strings.Fixed.Equal_Case_Insensitive
     (LC_String_1, UC_String_1) then

      Report.Failed
        ("Incorrect operation of function Fixed.Equal_Case_Insensitive LC" &
         " with UC - matching");

   end if;

   if not Ada.Strings.Fixed.Equal_Case_Insensitive
     (TC_String_1, UC_String_1) then

      Report.Failed
        ("Incorrect operation of function Fixed.qual_Case_Insensitive TC" &
         " with UC - matching");

   end if;

   -- Check non-matching strings of same length

   if Ada.Strings.Fixed.Equal_Case_Insensitive
     (LC_String_1, LC_String_2) then

      Report.Failed
        ("Incorrect operation of function Fixed.Equal_Case_Insensitive LC" &
         " - non-matching same length");

   end if;

   if Ada.Strings.Fixed.Equal_Case_Insensitive
     (TC_String_1, TC_String_2) then

      Report.Failed
        ("Incorrect operation of function Fixed.Equal_Case_Insensitive TC" &
         " - non-matching same length");

   end if;

   if Ada.Strings.Fixed.Equal_Case_Insensitive
     (UC_String_1, UC_String_2) then

      Report.Failed
        ("Incorrect operation of function Fixed.Equal_Case_Insensitive UC" &
         " - non-matching same length");

   end if;

   -- Check part-matching strings of different lengths

   if Ada.Strings.Fixed.Equal_Case_Insensitive
     (LC_String_1, LC_String_3) then

      Report.Failed
        ("Incorrect operation of function Fixed.Equal_Case_Insensitive LC" &
         " with longer LC - part-matching different lengths");

   end if;

   if Ada.Strings.Fixed.Equal_Case_Insensitive
     (TC_String_1, TC_String_3) then

      Report.Failed
        ("Incorrect operation of function Fixed.Equal_Case_Insensitive TC" &
         " with longer TC - part-matching different lengths");

   end if;

   if Ada.Strings.Fixed.Equal_Case_Insensitive
     (UC_String_1, UC_String_3) then

      Report.Failed
        ("Incorrect operation of function Fixed.Equal_Case_Insensitive UC" &
         " with longer UC - part-matching different lengths");

   end if;


   -- Bounded Strings

   -- Check matching strings of same length

   if not Bounded_6_Equal_Case_Insensitive
     (LC_Bounded_String_1, TC_Bounded_String_1) then

      Report.Failed
        ("Incorrect operation of function Bounded.Equal_Case_Insensitive LC" &
         " with TC - matching");

   end if;

   if not Bounded_6_Equal_Case_Insensitive
     (LC_Bounded_String_1, UC_Bounded_String_1) then

      Report.Failed
        ("Incorrect operation of function Bounded.Equal_Case_Insensitive LC" &
         " with UC - matching");

   end if;

   if not Bounded_6_Equal_Case_Insensitive
     (TC_Bounded_String_1, UC_Bounded_String_1) then

      Report.Failed
        ("Incorrect operation of function Bounded.Equal_Case_Insensitive TC" &
         " with UC - matching");

   end if;

   -- Check non-matching strings of same length

   if Bounded_6_Equal_Case_Insensitive
     (LC_Bounded_String_1, LC_Bounded_String_2) then

      Report.Failed
        ("Incorrect operation of function Bounded.Equal_Case_Insensitive LC" &
         " - non-matching same length");

   end if;

   if Bounded_6_Equal_Case_Insensitive
     (TC_Bounded_String_1, TC_Bounded_String_2) then

      Report.Failed
        ("Incorrect operation of function Bounded.Equal_Case_Insensitive TC" &
         " - non-matching same length");

   end if;

   if Bounded_6_Equal_Case_Insensitive
     (UC_Bounded_String_1, UC_Bounded_String_2) then

      Report.Failed
        ("Incorrect operation of function Bounded.Equal_Case_Insensitive UC" &
         " - non-matching same length");

   end if;

   -- Check part-matching strings of different lengths

   if Bounded_6_Equal_Case_Insensitive
     (LC_Bounded_String_1, LC_Bounded_String_3) then

      Report.Failed
        ("Incorrect operation of function Bounded.Equal_Case_Insensitive LC" &
         " with longer LC - part-matching different lengths");

   end if;

   if Bounded_6_Equal_Case_Insensitive
     (TC_Bounded_String_1, TC_Bounded_String_3) then

      Report.Failed
        ("Incorrect operation of function Bounded.Equal_Case_Insensitive TC" &
         " with longer TC - part-matching different lengths");

   end if;

   if Bounded_6_Equal_Case_Insensitive
     (UC_Bounded_String_1, UC_Bounded_String_3) then

      Report.Failed
        ("Incorrect operation of function Bounded.Equal_Case_Insensitive UC" &
         " with longer UC - part-matching different lengths");

   end if;


   -- Unbounded Strings

   -- Check matching strings of same length

   if not Ada.Strings.Unbounded.Equal_Case_Insensitive
     (LC_Unbounded_String_1, TC_Unbounded_String_1) then

      Report.Failed
        ("Incorrect operation of function Unbounded.Equal_Case_Insensitive LC" &
         " with TC - matching");

   end if;

   if not Ada.Strings.Unbounded.Equal_Case_Insensitive
     (LC_Unbounded_String_1, UC_Unbounded_String_1) then

      Report.Failed
        ("Incorrect operation of function Unbounded.Equal_Case_Insensitive LC" &
         " with UC - matching");

   end if;

   if not Ada.Strings.Unbounded.Equal_Case_Insensitive
     (TC_Unbounded_String_1, UC_Unbounded_String_1) then

      Report.Failed
        ("Incorrect operation of function Unbounded.Equal_Case_Insensitive TC" &
         " with UC - matching");

   end if;

   -- Check non-matching strings of same length

   if Ada.Strings.Unbounded.Equal_Case_Insensitive
     (LC_Unbounded_String_1, LC_Unbounded_String_2) then

      Report.Failed
        ("Incorrect operation of function Unbounded.Equal_Case_Insensitive LC" &
         " - non-matching same length");

   end if;

   if Ada.Strings.Unbounded.Equal_Case_Insensitive
     (TC_Unbounded_String_1, TC_Unbounded_String_2) then

      Report.Failed
        ("Incorrect operation of function Unbounded.Equal_Case_Insensitive TC" &
         " - non-matching same length");

   end if;

   if Ada.Strings.Unbounded.Equal_Case_Insensitive
     (UC_Unbounded_String_1, UC_Unbounded_String_2) then

      Report.Failed
        ("Incorrect operation of function Unbounded.Equal_Case_Insensitive UC" &
         " - non-matching same length");

   end if;

   -- Check part-matching strings of different lengths

   if Ada.Strings.Unbounded.Equal_Case_Insensitive
     (LC_Unbounded_String_1, LC_Unbounded_String_3) then

      Report.Failed
        ("Incorrect operation of function Unbounded.Equal_Case_Insensitive LC" &
         " with longer LC - part-matching different lengths");

   end if;

   if Ada.Strings.Unbounded.Equal_Case_Insensitive
     (TC_Unbounded_String_1, TC_Unbounded_String_3) then

      Report.Failed
        ("Incorrect operation of function Unbounded.Equal_Case_Insensitive TC" &
         " with longer TC - part-matching different lengths");

   end if;

   if Ada.Strings.Unbounded.Equal_Case_Insensitive
     (UC_Unbounded_String_1, UC_Unbounded_String_3) then

      Report.Failed
        ("Incorrect operation of function Unbounded.Equal_Case_Insensitive UC" &
         " with longer UC - part-matching different lengths");

   end if;


   -- Less_Case_Insensitive

   -- Strings

   -- Check matching strings of same length

   if Ada.Strings.Less_Case_Insensitive
     (LC_String_1, TC_String_1) then

      Report.Failed
        ("Incorrect operation of function Less_Case_Insensitive LC" &
         " with TC - matching");

   end if;

   if Ada.Strings.Less_Case_Insensitive
     (LC_String_1, UC_String_1) then

      Report.Failed
        ("Incorrect operation of function Less_Case_Insensitive LC" &
         " with UC - matching");

   end if;

   if Ada.Strings.Less_Case_Insensitive
     (TC_String_1, UC_String_1) then

      Report.Failed
        ("Incorrect operation of function Less_Case_Insensitive TC" &
         " with UC - matching");

   end if;


   -- Check non-matching strings of same length

   if not Ada.Strings.Less_Case_Insensitive
     (LC_String_1, LC_String_2) then

      Report.Failed
        ("Incorrect operation of function Less_Case_Insensitive LC" &
         " - non-matching same length");

   end if;

   if not Ada.Strings.Less_Case_Insensitive
     (TC_String_1, TC_String_2) then

      Report.Failed
        ("Incorrect operation of function Less_Case_Insensitive TC" &
         " - non-matching same length");

   end if;

   if not Ada.Strings.Less_Case_Insensitive
     (UC_String_1, UC_String_2) then

      Report.Failed
        ("Incorrect operation of function Less_Case_Insensitive UC" &
         " - non-matching same length");

   end if;


   -- Check part-matching strings of different lengths

   if not Ada.Strings.Less_Case_Insensitive
     (LC_String_1, LC_String_3) then

      Report.Failed
        ("Incorrect operation of function Less_Case_Insensitive LC" &
         " with longer LC - part-matching different lengths");

   end if;

   if not Ada.Strings.Less_Case_Insensitive
     (TC_String_1, TC_String_3) then

      Report.Failed
        ("Incorrect operation of function Less_Case_Insensitive TC" &
         " with longer TC - part-matching different lengths");

   end if;

   if not Ada.Strings.Less_Case_Insensitive
     (UC_String_1, UC_String_3) then

      Report.Failed
        ("Incorrect operation of function Less_Case_Insensitive UC" &
         " with longer UC - part-matching different lengths");

   end if;


   -- Fixed Strings

   -- Check matching strings of same length

   if Ada.Strings.Fixed.Less_Case_Insensitive
     (LC_String_1, TC_String_1) then

      Report.Failed
        ("Incorrect operation of function Less_Case_Insensitive LC" &
         " with TC - matching");

   end if;

   if Ada.Strings.Fixed.Less_Case_Insensitive
     (LC_String_1, UC_String_1) then

      Report.Failed
        ("Incorrect operation of function Less_Case_Insensitive LC" &
         " with UC - matching");

   end if;

   if Ada.Strings.Fixed.Less_Case_Insensitive
     (TC_String_1, UC_String_1) then

      Report.Failed
        ("Incorrect operation of function Less_Case_Insensitive TC" &
         " with UC - matching");

   end if;


   -- Check non-matching strings of same length

   if not Ada.Strings.Fixed.Less_Case_Insensitive
     (LC_String_1, LC_String_2) then

      Report.Failed
        ("Incorrect operation of function Less_Case_Insensitive LC" &
         " - non-matching same length");

   end if;

   if not Ada.Strings.Fixed.Less_Case_Insensitive
     (TC_String_1, TC_String_2) then

      Report.Failed
        ("Incorrect operation of function Less_Case_Insensitive TC" &
         " - non-matching same length");

   end if;

   if not Ada.Strings.Fixed.Less_Case_Insensitive
     (UC_String_1, UC_String_2) then

      Report.Failed
        ("Incorrect operation of function Less_Case_Insensitive UC" &
         " - non-matching same length");

   end if;


   -- Check part-matching strings of different lengths

   if not Ada.Strings.Fixed.Less_Case_Insensitive
     (LC_String_1, LC_String_3) then

      Report.Failed
        ("Incorrect operation of function Less_Case_Insensitive LC" &
         " with longer LC - part-matching different lengths");

   end if;

   if not Ada.Strings.Fixed.Less_Case_Insensitive
     (TC_String_1, TC_String_3) then

      Report.Failed
        ("Incorrect operation of function Less_Case_Insensitive TC" &
         " with longer TC - part-matching different lengths");

   end if;

   if not Ada.Strings.Fixed.Less_Case_Insensitive
     (UC_String_1, UC_String_3) then

      Report.Failed
        ("Incorrect operation of function Less_Case_Insensitive UC" &
         " with longer UC - part-matching different lengths");

   end if;


   -- Bounded Strings

   -- Check matching strings of same length

   if Bounded_6_Less_Case_Insensitive
     (LC_Bounded_String_1, TC_Bounded_String_1) then

      Report.Failed
        ("Incorrect operation of function Bounded.Less_Case_Insensitive LC" &
         " with TC - matching");

   end if;

   if Bounded_6_Less_Case_Insensitive
     (LC_Bounded_String_1, UC_Bounded_String_1) then

      Report.Failed
        ("Incorrect operation of function Bounded.Less_Case_Insensitive LC" &
         " with UC - matching");

   end if;

   if Bounded_6_Less_Case_Insensitive
     (TC_Bounded_String_1, UC_Bounded_String_1) then

      Report.Failed
        ("Incorrect operation of function Bounded.Less_Case_Insensitive TC" &
         " with UC - matching");

   end if;


   -- Check non-matching strings of same length

   if not Bounded_6_Less_Case_Insensitive
     (LC_Bounded_String_1, LC_Bounded_String_2) then

      Report.Failed
        ("Incorrect operation of function Bounded.Less_Case_Insensitive LC" &
         " - non-matching same length");

   end if;

   if not Bounded_6_Less_Case_Insensitive
     (TC_Bounded_String_1, TC_Bounded_String_2) then

      Report.Failed
        ("Incorrect operation of function Bounded.Less_Case_Insensitive TC" &
         " - non-matching same length");

   end if;

   if not Bounded_6_Less_Case_Insensitive
     (UC_Bounded_String_1, UC_Bounded_String_2) then

      Report.Failed
        ("Incorrect operation of function Bounded.Less_Case_Insensitive UC" &
         " - non-matching same length");

   end if;


   -- Check part-matching strings of different lengths

   if not Bounded_6_Less_Case_Insensitive
     (LC_Bounded_String_1, LC_Bounded_String_3) then

      Report.Failed
        ("Incorrect operation of function Bounded.Less_Case_Insensitive LC" &
         " with longer LC - part-matching different lengths");

   end if;

   if not Bounded_6_Less_Case_Insensitive
     (TC_Bounded_String_1, TC_Bounded_String_3) then

      Report.Failed
        ("Incorrect operation of function Bounded.Less_Case_Insensitive TC" &
         " with longer TC - part-matching different lengths");

   end if;

   if not Bounded_6_Less_Case_Insensitive
     (UC_Bounded_String_1, UC_Bounded_String_3) then

      Report.Failed
        ("Incorrect operation of function Bounded.Less_Case_Insensitive UC" &
         " with longer UC - part-matching different lengths");

   end if;


   -- Unbounded Strings

   -- Check matching strings of same length

   if Ada.Strings.Unbounded.Less_Case_Insensitive
     (LC_Unbounded_String_1, TC_Unbounded_String_1) then

      Report.Failed
        ("Incorrect operation of function Unbounded.Less_Case_Insensitive LC" &
         " with TC - matching");

   end if;

   if Ada.Strings.Unbounded.Less_Case_Insensitive
     (LC_Unbounded_String_1, UC_Unbounded_String_1) then

      Report.Failed
        ("Incorrect operation of function Unbounded.Less_Case_Insensitive LC" &
         " with UC - matching");

   end if;

   if Ada.Strings.Unbounded.Less_Case_Insensitive
     (TC_Unbounded_String_1, UC_Unbounded_String_1) then

      Report.Failed
        ("Incorrect operation of function Unbounded.Less_Case_Insensitive TC" &
         " with UC - matching");

   end if;


   -- Check non-matching strings of same length

   if not Ada.Strings.Unbounded.Less_Case_Insensitive
     (LC_Unbounded_String_1, LC_Unbounded_String_2) then

      Report.Failed
        ("Incorrect operation of function Unbounded.Less_Case_Insensitive LC" &
         " - non-matching same length");

   end if;

   if not Ada.Strings.Unbounded.Less_Case_Insensitive
     (TC_Unbounded_String_1, TC_Unbounded_String_2) then

      Report.Failed
        ("Incorrect operation of function Bounded.Less_Case_Insensitive TC" &
         " - non-matching same length");

   end if;

   if not Ada.Strings.Unbounded.Less_Case_Insensitive
     (UC_Unbounded_String_1, UC_Unbounded_String_2) then

      Report.Failed
        ("Incorrect operation of function Unbounded.Less_Case_Insensitive UC" &
         " - non-matching same length");

   end if;


   -- Check part-matching strings of different lengths

   if not Ada.Strings.Unbounded.Less_Case_Insensitive
     (LC_Unbounded_String_1, LC_Unbounded_String_3) then

      Report.Failed
        ("Incorrect operation of function Unbounded.Less_Case_Insensitive LC" &
         " with longer LC - part-matching different lengths");

   end if;

   if not Ada.Strings.Unbounded.Less_Case_Insensitive
     (TC_Unbounded_String_1, TC_Unbounded_String_3) then

      Report.Failed
        ("Incorrect operation of function Unbounded.Less_Case_Insensitive TC" &
         " with longer TC - part-matching different lengths");

   end if;

   if not Ada.Strings.Unbounded.Less_Case_Insensitive
     (UC_Unbounded_String_1, UC_Unbounded_String_3) then

      Report.Failed
        ("Incorrect operation of function Unbounded.Less_Case_Insensitive UC" &
         " with longer UC - part-matching different lengths");

   end if;


   Report.Result;

end CXA4035;
