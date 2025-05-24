-- CA110230.A
--
--                             Grant of Unlimited Rights
--
--     AdaCore holds unlimited rights in the software and documentation
--     contained herein. Unlimited rights are the same as those granted
--     by the U.S. Government for older parts of the Ada Conformity
--     Assessment Test Suite, and are defined in DFAR 252.227-7013(a)(19).
--     By making this public release, AdaCore intends to confer upon all
--     recipients unlimited rights equal to those held by the Ada Conformity
--     Assessment Authority. These rights include rights to use, duplicate,
--     release or disclose the released technical data and computer software
--     in whole or in part, in any manner and for any purpose whatsoever,
--     and to have or permit others to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS. ADACORE MAKES NO EXPRESS OR IMPLIED WARRANTY AS
--     TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE SOFTWARE,
--     DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE OR
--     DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
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
--     This test is based on one submitted by AdaCore; AdaCore retains the
--     copyright on the test.
--*
-- OBJECTIVE:
--     Check that the context clause of a limited view is empty.
--
--     Check that types imported from a limited view appear complete
--     when the library package is visible.
--
-- TEST DESCRIPTION:
--
--     We test that limited with can be used for its intended purpose of
--     allowing mutually-dependent types between two library package
--     specifications.
--
--     This test is based in the example discussed in AI95-00217-6.
--
-- SPECIAL REQUIREMENTS:
--      To build this test:
--         1) Do the steps required to add the limited view of the file
--            CA110230 to the compilation environment. (Careful: the
--            compilation of the normal view of the file CA110230 should
--            not be done at this time.)
--         2) Do the steps required to add the limited view of the file
--            CA110231 to the environment.
--         3) Compile the file CA110230 (and add the results to the
--            environment).
--         4) Compile the file CA110231 (and add the results to the
--            environment).
--         5) Compile the file CA110232 (and add the results to the
--            environment).
--         6) Build an executable image and run it.
--
-- TEST FILES:
--      This test consists of the following files:
--      -> CA110230.A
--         CA110231.A
--         CA110232.AM
--
-- CHANGE HISTORY:
--     9 Feb 2004 JM  Initial Version.
--     9 Apr 2007 RLB Added test objective, reordered units, added
--                    failure checks.
--    25 Apr 2007 RLB Split into separate files so that the various units
--                    can be added to the environment independently. Added
--                    special requirements to make it clear when the limited
--                    views need to be added to the environment.
--     6 Sep 2007 RLB Corrected mode on Create.
--
--!
limited with CA11023E;
package CA11023D is
   type Emp_Ptr is access all CA11023E.Employee'Class;
   type Department is tagged private;

   procedure Choose_Manager
     (D       : in out Department;
      Manager :    out Emp_Ptr);

   procedure Create (D : in out Department; Name : in String);

   procedure Display (D : in Department);

   procedure Appoint
    (E : Emp_Ptr;
     D : in out Department);

   function Is_Member
    (E : Emp_Ptr;
     D : in Department) return Boolean;

private
   subtype Dept_Name is String (1 .. 13);
   type T_List is array (Positive range <>) of Emp_Ptr;

   type Department is tagged record
      Id   : Dept_Name := "<<Dept_Name>>";
      List : T_List (1 .. 5);
      Tot  : Natural := 0;
   end record;
end CA11023D;

