-- C611B033.AM
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
--     See C611B030.A.
--
-- TEST DESCRIPTION:
--     See C611B030.A.
--
-- TEST FILES:
--     This test consists of the following files:
--         C611B030.A
--         C611B031.A
--         C611B032.A
--      -> C611B033.AM
--
-- CHANGE HISTORY:
--      27 Dec 16   JAC     Initial pre-release version.
--      25 Jan 17   RLB     Corrected name of main subprogram.
--      24 Mar 17   RLB     Corrected too long lines.
--
--!
with Ada.Tags;
with C611B030.Child.Grandchild;
with F611B00;
with Report;

procedure C611B033 is

   pragma Assertion_Policy (Check);

   use type Ada.Tags.Tag;

   My_Root_Event            : C611B030.Child.Event_Record;
   My_Annotated_Event       : C611B030.Child.Grandchild.Annotated_Event_Record;
   My_First_Classwide_Event : C611B030.Child.Event_Record'Class :=
                                  My_Annotated_Event;
   My_Second_Classwide_Event: C611B030.Child.Event_Record'Class :=
                                  My_Root_Event;

begin

   Report.Test
     ("C611B03",
      "For X'Old given in the postcondition for a subprogram S, check that " &
      "X'Old has the same tag as X when X is a parameter P of S, even if " &
      "the tag of X is different than the nominal subtype of P (Part 2: " &
      "Post'Class)");

   F611B00.TC_Clear;

   C611B030.Child.Event_Occurred (
      C611B030.Child.Event_Record (My_First_Classwide_Event));

   -- Tag of the parameter is of Annotated_Event_Record, but the call
   -- is statically bound to the primitive of type Event_Record.
   -- Thus (by AI12-0113-1) the Count primitives of Event_Record should
   -- be called. However, if the tag of the parameter of the Count primitives
   -- is queried, it should be Annotated_Event_Record (for both Event and
   -- Event'Old).

   if not F611B00.TC_Event_Record_Count1_Called then
      Report.Failed
       ("Event function 1 not called for Annotated_Event");
      --F611B00.TC_Output;
   end if;

   if F611B00.TC_Annotated_Event_Record_Count1_Called then
      Report.Failed
       ("Annotated Event function 1 called for Annotated_Event");
      --F611B00.TC_Output;
   end if;

   if F611B00.TC_Event_Record_Count1_Tag /= My_First_Classwide_Event'Tag then
      Report.Failed
       ("Wrong tag for parameter of Count1 primitive function");
   end if;

   if not F611B00.TC_Event_Record_Count2_Called then
      Report.Failed
       ("Event function 2 not called for Annotated_Event");
      --F611B00.TC_Output;
   end if;

   if F611B00.TC_Annotated_Event_Record_Count2_Called then
      Report.Failed
       ("Annotated Event function 2 called for Annotated_Event");
      --F611B00.TC_Output;
   end if;

   if F611B00.TC_Event_Record_Count2_Tag /= My_First_Classwide_Event'Tag then
      Report.Failed
       ("Wrong tag for parameter of Count2 primitive function");
   end if;

   F611B00.TC_Clear;

   C611B030.Child.Event_Occurred (
      C611B030.Child.Event_Record (My_Second_Classwide_Event));

   -- Here the Tag and nominal subtype are both Event_Record.

   if not F611B00.TC_Event_Record_Count1_Called then
      Report.Failed
       ("Event function 1 not called for Root_Event");
      --F611B00.TC_Output;
   end if;

   if F611B00.TC_Annotated_Event_Record_Count1_Called then
      Report.Failed
       ("Annotated Event function 1 called for Root_Event");
      --F611B00.TC_Output;
   end if;

   if F611B00.TC_Event_Record_Count1_Tag /= My_Second_Classwide_Event'Tag then
      Report.Failed
       ("Wrong tag for root event parameter of Count1 primitive function");
   end if;

   if not F611B00.TC_Event_Record_Count2_Called then
      Report.Failed
       ("Event function 2 not called for Root_Event");
      --F611B00.TC_Output;
   end if;

   if F611B00.TC_Annotated_Event_Record_Count2_Called then
      Report.Failed
       ("Annotated Event function 2 called for Root_Event");
      --F611B00.TC_Output;
   end if;

   if F611B00.TC_Event_Record_Count2_Tag /= My_Second_Classwide_Event'Tag then
      Report.Failed
       ("Wrong tag for root event parameter of Count2 primitive function");
   end if;

   Report.Result;

end C611B033;
