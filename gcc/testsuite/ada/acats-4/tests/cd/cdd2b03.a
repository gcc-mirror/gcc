-- CDD2B03.A
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
--    The ACAA has created and maintains the Ada Conformity Assessment Test
--    Suite for the purpose of conformity assessments conducted in accordance
--    with the International Standard ISO/IEC 18009 - Ada: Conformity
--    assessment of a language processor. This test suite should not be used
--    to make claims of conformance unless used in accordance with
--    ISO/IEC 18009 and any applicable ACAA procedures.
--
--*
--
-- OBJECTIVE:
--      Check that the default stream attributes read and write the number
--      of stream elements specified by their Stream_Size attribute. Part 3:
--      attribute definition clauses, non-confirming values.
--
-- TEST DESCRIPTION:
--      This test is designed to demonstrate using streaming with an existing
--      protocol where the size of items is specified.
--
--      We use the foundation to define a memory buffer stream to emulate the
--      communication channel, and then check that the correct number of
--      stream elements is read and written by each default stream attribute
--      usage.
--
--      We assume the reading and writing sides were developed independently,
--      so different data types may have been used to construct them. (We don't
--      try to organize the code to show the actual construction of the
--      different sides.)
--
--      This test assumes that Stream_Element'Size is 8 or larger and that
--      signed integers can be defined that use at least 4 Stream_Elements.
--      The test is strongest for implementations where Stream_Element'Size is
--      exactly 8; other implementations can use the test but some test cases
--      will be ineffective.
--
-- APPLICABILITY CRITERIA:
--      All implementations must attempt to compile this test.
--
--      For an implementation where Stream_Element'Size is less than 8, or
--        for which the largest signed integer uses less than 4 stream
--        elements, this test may report compile time errors at one or more
--        points indicated by "-- N/A => ERROR", in which case it may be graded
--        as inapplicable. Otherwise, the test must execute and report PASSED.
--
--      For other implementations validating against Systems Programming
--        Annex (C):
--        this test must execute and report PASSED.
--
--      For other implementations not validating against Annex C:
--        this test may report compile time errors at one or more points
--        indicated by "-- ANX-C RQMT", in which case it may be graded as
--        inapplicable. Otherwise, the test must execute and report PASSED.
--
--
-- CHANGE HISTORY:
--     19 Mar 20   RLB     Created test.
--
--!

with Ada.Streams;
with Report;
with FDD2B00;
procedure CDD2B03 is

   Stream_Element_Size : constant := Ada.Streams.Stream_Element'Size;

   -- We first declare a pair of types to check if the Stream_Element_Size
   -- is at least 8, and if we can declare integers to stream 4 elements.
   -- If either of these fail, the test is not applicable (although an
   -- implementation validating Annex C and that meets the requirements
   -- should not report errors here).

   type Basic_Int is range -99 .. 99;
   for Basic_Int'Stream_Size use Stream_Element_Size*1; -- N/A => ERROR. {4;1}

   type Big_Int is range -999999 .. 999999;
   for Big_Int'Stream_Size use Stream_Element_Size*4;   -- N/A => ERROR. {4;1}

   -- Now, we declare a number of types with non-confirming stream sizes
   -- allowed by the recommended level of support.

   type Unsigned is range 0 .. 199;
   for Unsigned'Stream_Size use Stream_Element_Size*1;  -- ANX-C RQMT. {4;1}
      -- This type must be streamed using an unsigned representation if
      -- Stream_Element_Size = 8.

   type Big_Stream is range -9999 .. 9999;
   for Big_Stream'Stream_Size use Stream_Element_Size*4;-- ANX-C RQMT. {4;1}
      -- This type only needs 2 stream elements, but we're requesting that
      -- 4 be used.

   type Odd_Stream is range -9999 .. 9999;
   for Odd_Stream'Stream_Size use Stream_Element_Size*3;-- ANX-C RQMT. {4;1}
      -- This type only needs 2 stream elements, but we're requesting that
      -- 3 be used. Note that streaming a 24-bit representation is required by
      -- the Recommended Level of Support for any implementation with a 32-bit
      -- integer type.

   type Rec is record
      A : Unsigned;
      B : Basic_Int;
      C : Big_Stream;
   end record;

   type Arr is array (1 .. 2) of Odd_Stream;

   The_Channel : aliased FDD2B00.Channel_Type;

   use type Ada.Streams.Stream_Element_Offset;

begin
   Report.Test ("CDD2B03", "Check that the default stream attributes read " &
                           "and write the specified number of stream " &
                           "elements. Part 3: attribute definition clauses " &
                           "with non-confirming stream sizes");

   Report.Comment ("Stream_Element'Size=" &
       Natural'Image(Ada.Streams.Stream_Element'Size));

   if FDD2B00.Element_Count (The_Channel) /= 0 then
      Report.Failed ("Channel not properly initialized");
   end if;
   FDD2B00.Clear (The_Channel);
   if FDD2B00.Element_Count (The_Channel) /= 0 then
      Report.Failed ("Channel clear failed");
   end if;

   -- The data is an unsigned integer, read and written individually.
   declare
      Foo : Unsigned := 12;
      Bar : Unsigned;
   begin
      Unsigned'Write (The_Channel'Access, Foo);
      if FDD2B00.Element_Count (The_Channel) /= 1 then
         Report.Failed ("Wrong number of stream elements written (1)");
      else
          Unsigned'Read (The_Channel'Access, Bar);
          if Foo /= Bar then
             Report.Failed ("Wrong value read (1)");
          elsif FDD2B00.Element_Count (The_Channel) /= 0 then
             Report.Failed ("Wrong number of stream elements read (1)");
          -- else OK.
          end if;
      end if;
   end;

   -- The data is a pair of values with different stream reps, read and
   -- written individually.
   FDD2B00.Clear (The_Channel);
   declare
      OSS : Odd_Stream := 4337;
      OSR : Odd_Stream;
      BSS : Big_Stream := 621;
      BSR : Big_Stream;
   begin
      Odd_Stream'Write (The_Channel'Access, OSS);
      Big_Stream'Write (The_Channel'Access, BSS);
      if FDD2B00.Element_Count (The_Channel) /= 7 then
         Report.Failed ("Wrong number of stream elements written (2)");
      else
          Odd_Stream'Read (The_Channel'Access, OSR);
          if OSS /= OSR then
             Report.Failed ("Wrong value read (2A)");
          elsif FDD2B00.Element_Count (The_Channel) /= 4 then
             Report.Failed ("Wrong number of stream elements read (2A)");
          else
             Big_Stream'Read (The_Channel'Access, BSR);
             if BSS /= BSR then
                Report.Failed ("Wrong value read (2B)");
             elsif FDD2B00.Element_Count (The_Channel) /= 0 then
                Report.Failed ("Wrong number of stream elements read (2B)");
             -- else OK.
             end if;
          end if;
      end if;
   end;

   -- The data is a triple of integers with different stream reps, written
   -- as a record on the sender side of the app, read individually on the
   -- receiver side.
   FDD2B00.Clear (The_Channel);
   declare
      My_Rec : Rec := (4, -2, 1522);
      URR : Unsigned;
      BIR : Basic_Int;
      BSR : Big_Stream;
   begin
      Rec'Write (The_Channel'Access, My_Rec);
         -- Must write the components in their defined stream size and
         -- declaration order. We'll read them back individually.
      if FDD2B00.Element_Count (The_Channel) /= 6 then
         Report.Failed ("Wrong number of stream elements written (3)");
      else
          Unsigned'Read (The_Channel'Access, URR);
          Basic_Int'Read (The_Channel'Access, BIR);
          Big_Stream'Read (The_Channel'Access, BSR);
          if My_Rec.A /= URR or else
             My_Rec.B /= BIR or else
             My_Rec.C /= BSR then
             Report.Failed ("Wrong values read (3)");
          elsif FDD2B00.Element_Count (The_Channel) /= 0 then
             Report.Failed ("Wrong number of stream elements read (3)");
          -- else OK.
          end if;
      end if;
   end;

   -- The data is a pair of integers with different stream reps, written
   -- individually on the sender side of the app, read as an array on the
   -- receiver side.
   FDD2B00.Clear (The_Channel);
   declare
      OS1 : Odd_Stream := 2318;
      OS2 : Odd_Stream := 4337;
      My_Arr : Arr;
   begin
      Odd_Stream'Write (The_Channel'Access, OS1);
      Odd_Stream'Write (The_Channel'Access, OS2);
      if FDD2B00.Element_Count (The_Channel) /= 6 then
         Report.Failed ("Wrong number of stream elements written (4)");
      else
          Arr'Read (The_Channel'Access, My_Arr);
          -- Should read each component in its specified stream size, in
          -- index order.
          if My_Arr(1) /= OS1 or else My_Arr(2) /= OS2 then
             Report.Failed ("Wrong values read (4)");
          elsif FDD2B00.Element_Count (The_Channel) /= 0 then
             Report.Failed ("Wrong number of stream elements read (4)");
          --else OK.
          end if;
      end if;
   end;

   Report.Result;
end CDD2B03;
