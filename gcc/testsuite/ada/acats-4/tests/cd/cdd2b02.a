-- CDD2B02.A
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
--      of stream elements specified by their Stream_Size attribute. Part 2:
--      aspect specifications, confirming values.
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
-- APPLICABILITY CRITERIA:
--      All implementations must attempt to compile this test.
--
--      For implementations validating against Systems Programming Annex (C):
--        this test must execute and report PASSED.
--
--      For implementations not validating against Annex C:
--        this test may report compile time errors at one or more points
--        indicated by "-- ANX-C RQMT", in which case it may be graded as
--        inapplicable. Otherwise, the test must execute and report PASSED.
--
--
-- CHANGE HISTORY:
--     17 Mar 20   RLB     Created test.
--
--!

with Ada.Streams;
with Report;
with FDD2B00;
procedure CDD2B02 is

   Stream_Element_Size : constant := Ada.Streams.Stream_Element'Size;
   type Basic_Int is range -99 .. 99;
   Basic_Stream_Size : constant := Basic_Int'Stream_Size / Stream_Element_Size;
   type Larger_Int is range -9999 .. 9999;
   Larger_Stream_Size : constant :=
                                Larger_Int'Stream_Size / Stream_Element_Size;
   type Basic_Mod is mod 2 ** 8;
   Mod_Stream_Size : constant := Basic_Mod'Stream_Size / Stream_Element_Size;
   type Basic_Fix is delta 0.25 range -4.0 .. 4.0
      with Small => 0.25;
   Fix_Stream_Size : constant := Basic_Fix'Stream_Size / Stream_Element_Size;
   type Basic_Flt is digits 3;
   Flt_Stream_Size : constant := Basic_Flt'Stream_Size / Stream_Element_Size;

   -- Declare types with what should be confirming stream sizes; this should
   -- work on virtually all implementations, regardless of how unusual.

   type Small_Int is range -99 .. 99
       with Stream_Size => Basic_Int'Stream_Size;       -- ANX-C RQMT. {1:4;1}

   type Big_Stream is range -9999 .. 9999
       with Stream_Size => Larger_Int'Stream_Size;      -- ANX-C RQMT. {1:4;1}

   type Small_Mod is mod 2 ** 8
       with Stream_Size => Basic_Mod'Stream_Size;       -- ANX-C RQMT. {1:4;1}

   type Quarter_Fix is delta 0.25 range -4.0 .. 4.0
      with Small => 0.25,
           Stream_Size => Basic_Fix'Stream_Size;        -- ANX-C RQMT. {2:4;1}

   type Small_Flt is digits 3
      with Stream_Size => Basic_Flt'Stream_Size;        -- ANX-C RQMT. {1:4;1}

   -- Note: We won't try access types since they're usually not meaningful
   -- when streamed.

   The_Channel : aliased FDD2B00.Channel_Type;

   use type Ada.Streams.Stream_Element_Offset;

begin
   Report.Test ("CDD2B02", "Check that the default stream attributes read " &
                           "and write the specified number of stream " &
                           "elements. Part 2: aspect specifications with " &
                           "confirming stream sizes");

   Report.Comment ("Stream_Element'Size=" &
       Natural'Image(Ada.Streams.Stream_Element'Size));
   Report.Comment ("Small_Int'Stream_Size=" &
       Natural'Image(Small_Int'Stream_Size));
   Report.Comment ("Big_Stream'Stream_Size=" &
       Natural'Image(Big_Stream'Stream_Size));
   Report.Comment ("Small_Mod'Stream_Size=" &
       Natural'Image(Small_Mod'Stream_Size));
   Report.Comment ("Quarter_Fix'Stream_Size=" &
       Natural'Image(Quarter_Fix'Stream_Size));
   Report.Comment ("Small_Flt'Stream_Size=" &
       Natural'Image(Small_Flt'Stream_Size));

   if Small_Int'Stream_Size /= Stream_Element_Size * Basic_Stream_Size then
      Report.Failed ("Small_Int'Stream_Size not multiple of stream " &
                     "element size");
   end if;
   if Big_Stream'Stream_Size /= Stream_Element_Size * Larger_Stream_Size then
      Report.Failed ("Big_Stream'Stream_Size not multiple of stream " &
                     "element size");
   end if;
   if Small_Mod'Stream_Size /= Stream_Element_Size * Mod_Stream_Size then
      Report.Failed ("Small_Mod'Stream_Size not multiple of stream " &
                     "element size");
   end if;
   if Quarter_Fix'Stream_Size /= Stream_Element_Size * Fix_Stream_Size then
      Report.Failed ("Quarter_Fix'Stream_Size not multiple of stream " &
                     "element size");
   end if;
   if Small_Flt'Stream_Size /= Stream_Element_Size * Flt_Stream_Size then
      Report.Failed ("Small_Flt'Stream_Size not multiple of stream " &
                     "element size");
   end if;
   if FDD2B00.Element_Count (The_Channel) /= 0 then
      Report.Failed ("Channel not properly initialized");
   end if;
   FDD2B00.Clear (The_Channel);
   if FDD2B00.Element_Count (The_Channel) /= 0 then
      Report.Failed ("Channel clear failed");
   end if;

   -- The data is a basic integer, read and written individually.
   declare
      Foo : Small_Int := 12;
      Bar : Small_Int;
   begin
      Small_Int'Write (The_Channel'Access, Foo);
      if FDD2B00.Element_Count (The_Channel) /= Basic_Stream_Size then
         Report.Failed ("Wrong number of stream elements written (1)");
      else
          Small_Int'Read (The_Channel'Access, Bar);
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
      SMS : Small_Mod := 4;
      SMR : Small_Mod;
      BSS : Big_Stream := 621;
      BSR : Big_Stream;
   begin
      Small_Mod'Write (The_Channel'Access, SMS);
      Big_Stream'Write (The_Channel'Access, BSS);
      if FDD2B00.Element_Count (The_Channel) /=
                           Mod_Stream_Size + Larger_Stream_Size then
         Report.Failed ("Wrong number of stream elements written (2)");
      else
          Small_Mod'Read (The_Channel'Access, SMR);
          if SMS /= SMR then
             Report.Failed ("Wrong value read (2A)");
          elsif FDD2B00.Element_Count (The_Channel) /= Larger_Stream_Size then
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

   -- The data is four values, read and written individually.
   FDD2B00.Clear (The_Channel);
   declare
      SMS : Small_Mod := 92;
      SMR : Small_Mod;
      BSS : Big_Stream := 4337;
      BSR : Big_Stream;
      QFS : Quarter_Fix := -1.5;
      QFR : Quarter_Fix;
      SFS : Small_Flt := -3.25;
      SFR : Small_Flt;
   begin
      Quarter_Fix'Write (The_Channel'Access, QFS);
      Big_Stream'Write (The_Channel'Access, BSS);
      Small_Flt'Write (The_Channel'Access, SFS);
      Small_Mod'Write (The_Channel'Access, SMS);
      if FDD2B00.Element_Count (The_Channel) /=
         Fix_Stream_Size + Larger_Stream_Size +
         Flt_Stream_Size + Mod_Stream_Size then
         Report.Failed ("Wrong number of stream elements written (3)");
      else
          Quarter_Fix'Read (The_Channel'Access, QFR);
          if QFS /= QFR then
             Report.Failed ("Wrong value read (2A)");
          elsif FDD2B00.Element_Count (The_Channel) /=
             Larger_Stream_Size + Flt_Stream_Size + Mod_Stream_Size then
             Report.Failed ("Wrong number of stream elements read (3A)");
          else
             Big_Stream'Read (The_Channel'Access, BSR);
             if BSS /= BSR then
                Report.Failed ("Wrong value read (3B)");
             elsif FDD2B00.Element_Count (The_Channel) /=
                 Flt_Stream_Size + Mod_Stream_Size then
                Report.Failed ("Wrong number of stream elements read (3B)");
             else
                Small_Flt'Read (The_Channel'Access, SFR);
                if SFS /= SFR then
                   Report.Failed ("Wrong value read (3C)");
                elsif FDD2B00.Element_Count (The_Channel) /=
                                                      Mod_Stream_Size then
                   Report.Failed ("Wrong number of stream elements read (3C)");
                else
                   Small_Mod'Read (The_Channel'Access, SMR);
                   if SMS /= SMR then
                      Report.Failed ("Wrong value read (3D)");
                   elsif FDD2B00.Element_Count (The_Channel) /= 0 then
                      Report.Failed ("Wrong number of stream elements " &
                                     "read (3D)");
                   -- else OK.
                   end if;
                end if;
             end if;
          end if;
      end if;
   end;

   Report.Result;
end CDD2B02;
