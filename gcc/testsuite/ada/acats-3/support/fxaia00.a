-- FXAIA00.A
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
--
--*
--
-- FOUNDATION DESCRIPTION:
--      This package declares types and constants which represent a ragged
--      array of string values. These values are used to exercise
--      (indefinite) containers with the element type String.
--
-- CHANGE HISTORY:
--       6 Dec 13   JAC     Second pre-release version.
--      30 Dec 13   RLB     Moved common code to foundation.
--
--!

with Ada.Containers;
package FXAIA00 is

   Num_Tests : constant := 10;

   subtype Array_Bounds_Type is Ada.Containers.Count_Type range 1 .. Num_Tests;

   -- Values in increasing order as this is what determines the order when
   -- sorting and for sets.

   -- Minimimum of two characters required for the Replace_Element subtest.

   Value_In_1  : aliased constant String := "00";
   Value_In_2  : aliased constant String := "111";
   Value_In_3  : aliased constant String := "2222";
   Value_In_4  : aliased constant String := "33333";
   Value_In_5  : aliased constant String := "444444";
   Value_In_6  : aliased constant String := "5555555";
   Value_In_7  : aliased constant String := "66666666";
   Value_In_8  : aliased constant String := "777777777";
   Value_In_9  : aliased constant String := "8888888888";
   Value_In_10 : aliased constant String := "99999999999";

   type Value_In_Ptr_Type is access constant String;

   type Value_In_Ptr_Array_Type is array (Array_Bounds_Type)
      of Value_In_Ptr_Type;

   Value_In_Ptr_Array : constant Value_In_Ptr_Array_Type
     := (Value_In_1'Access, Value_In_2'Access, Value_In_3'Access,
         Value_In_4'Access, Value_In_5'Access, Value_In_6'Access,
         Value_In_7'Access, Value_In_8'Access, Value_In_9'Access,
         Value_In_10'Access);

end FXAIA00;
