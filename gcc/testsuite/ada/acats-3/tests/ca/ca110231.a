-- CA110231.A
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
--      See CA110230.A.
--
-- TEST DESCRIPTION:
--      See CA110230.A.
--
-- SPECIAL REQUIREMENTS:
--      See CA110230.A.
--
-- TEST FILES:
--      This test consists of the following files:
--         CA110230.A
--      -> CA110231.A
--         CA110232.AM
--
--  CHANGE HISTORY:
--     9 Feb 2004 JM  Initial Version.
--     9 Apr 2007 RLB Added test objective, reordered units, added
--                    failure checks.
--    25 Apr 2007 RLB Split into separate files so that the various units
--                    can be added to the environment independently. Added
--                    special requirements to make it clear when the limited
--                    views need to be added to the environment.
--
--!
limited with CA11023D;
package CA11023E is
   type Dept_Ptr is access all CA11023D.Department'Class;
   type Employee is tagged private;

   type Emp_Ptr is access all Employee'Class; -- used by function 'hire'

   procedure Assign_Employee
     (E : in out Employee;
      D : in     Dept_Ptr);

   function Current_Department
     (E : in Employee) return Dept_Ptr;

   function Hire (Id : String) return Emp_Ptr;

   procedure Display (E : Employee);
private
   type Employee is tagged record
      Dept : Dept_Ptr;
      Id   : access String;
   end record;
end CA11023E;

