-- C3A10031.A
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
--      See C3A10030.A.
--
-- TEST DESCRIPTION:
--      See C3A10030.A.
--
-- SPECIAL REQUIREMENTS:
--      See C3A10030.A.
--
-- TEST FILES:
--      This test consists of the following files:
--         C3A10030.A
--      -> C3A10031.A
--         C3A10032.AM
--
--  CHANGE HISTORY:
--    12 Jan 2015 RLB Created test, using example of AI95-0217-6 as a basis.
--
--!
limited with C3A1003D;
package C3A1003E is -- Employees
   type Dept_Ptr is access all C3A1003D.Department'Class;
   type Employee is tagged private;

   type Emp_Ptr is access all Employee'Class; -- used by function 'hire'

   procedure Assign_Employee
     (E : in out Employee;
      D : in out C3A1003D.Department'Class);

   function Current_Department
     (E : in Employee) return Dept_Ptr;

   function Hire (Id : String) return Emp_Ptr;

   procedure Display (E : Employee);
private
   type Employee is tagged record
      Dept : Dept_Ptr;
      Id   : access String;
      During_Mod : Boolean := False;
   end record;
end C3A1003E;

