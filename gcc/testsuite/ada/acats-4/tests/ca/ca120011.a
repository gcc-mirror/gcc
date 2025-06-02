-- CA120011.A
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
--  OBJECTIVE:
--      See CA120010.A.
--
--  TEST DESCRIPTION:
--      See CA120010.A.
--
--  SPECIAL REQUIREMENTS:
--      See CA120010.A.
--
--  TEST FILES:
--      This test consists of the following files:
--         CA120010.A
--      -> CA120011.A
--         CA120012.AM
--
--  CHANGE HISTORY:
--    11 Apr 2007 RLB Created test.
--    25 Apr 2007 RLB Split into separate files so that the various units
--                    can be added to the environment independently. Added
--                    special requirements to make it clear when the limited
--                    views need to be added to the environment.
--
--!

private package CA12001_Win.Impl is
    -- Low-level (system-dependent) implementation of windowing.

    type Low_Win_Type is private;

    procedure Open (Win : in out Low_Win_Type);

    procedure Close (Win : in out Low_Win_Type);

    procedure Set_Size (Win : in out Low_Win_Type;
                        Width  : in Window_Size;
                        Height : in Window_Size);

    procedure Get_Size (Win : in Low_Win_Type;
                        Width  : out Window_Size;
                        Height : out Window_Size);

private
    type Low_Win_Type is record
       -- In a real system, this would hold a window handle and similar
       -- information.
       Width  : Window_Size;
       Height : Window_Size;
    end record;
end CA12001_Win.Impl;

