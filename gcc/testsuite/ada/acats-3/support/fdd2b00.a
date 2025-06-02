-- FDD2B00.A
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
-- FOUNDATION DESCRIPTION:
--      This foundation emulates a network stream "channel", to be used to
--      test various stream attributes. Since we don't care about the
--      physical transport mechanism, we use a simple memory buffer.
--
--      Any resemblance to the proposed Storage_Streams of Ada 202x is purely
--      intentional. :-)
--
-- CHANGE HISTORY:
--     19 Mar 20   RLB     Created foundation.
--
--!

-- Create a package much like Ada.Streams.Storage.Bounded. Test the number
-- of stream elements read and written.


with Ada.Streams;
package FDD2B00 is
   pragma Pure(FDD2B00);

   type Channel_Type is new Ada.Streams.Root_Stream_Type with private;

   overriding
   procedure Read (
      Stream : in out Channel_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);

   overriding
   procedure Write (
      Stream : in out Channel_Type;
      Item   : in Ada.Streams.Stream_Element_Array);

   function Element_Count (Stream : Channel_Type)
      return Ada.Streams.Stream_Element_Count;

   procedure Clear (Stream : in out Channel_Type);

private
   type Channel_Type is new Ada.Streams.Root_Stream_Type with record
      Count  : Ada.Streams.Stream_Element_Count := 0;
      Buffer : Ada.Streams.Stream_Element_Array (1 .. 100);
   end record;
end FDD2B00;


package body FDD2B00 is

   procedure Read (
      Stream : in out Channel_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset) is
      use type Ada.Streams.Stream_Element_Offset;
   begin
      if Stream.Count <= Item'Length then
         Last := Item'First + Stream.Count - 1;
         Item(Item'First .. Last) := Stream.Buffer(1 .. Stream.Count);
         Stream.Count := 0;
      else
         Last := Item'Last;
         Item := Stream.Buffer(1 .. Item'Length);
         Stream.Count := Stream.Count - Item'Length;
         Stream.Buffer(1 .. Stream.Count) :=
             Stream.Buffer(Item'Length + 1 .. Stream.Count + Item'Length);
      end if;
   end Read;


   procedure Write (
      Stream : in out Channel_Type;
      Item   : in Ada.Streams.Stream_Element_Array) is
      use type Ada.Streams.Stream_Element_Offset;
   begin
      if Stream.Count + Item'Length > 100 then
         raise Constraint_Error with "Buffer full";
      end if;
      Stream.Buffer (Stream.Count + 1 .. Stream.Count + Item'Length) :=
         Item;
      Stream.Count := Stream.Count + Item'Length;
   end Write;


   function Element_Count (Stream : Channel_Type)
      return Ada.Streams.Stream_Element_Count is
   begin
      return Stream.Count;
   end Element_Count;


   procedure Clear (Stream : in out Channel_Type) is
   begin
      Stream.Count := 0;
   end Clear;

end FDD2B00;



