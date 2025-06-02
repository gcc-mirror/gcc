-- C3A10040.A
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
-- OBJECTIVE:
--     Check that a parameter of a tagged incomplete type can
--     be passed directly as a parameter.
--
--     Check that the name of a tagged incomplete view can be used as the
--     subtype_mark of a parameter in a subprogram_body.
--
--     Check that the name of a tagged incomplete view can be used as the
--     prefix of 'Class when that is used in a parameter context allowed for
--     a tagged incomplete view.
--
-- TEST DESCRIPTION:
--
--     We test that limited with can be used for its intended purpose of
--     allowing mutually-dependent types between two library package
--     specifications, and that a child package of one of them need not
--     (fully) with a package containing a type that it is not interested in.
--
--     We use the classic Employee and Department example as outlined in
--     AI95-00326-1. In this example, we have a child package containing
--     employee operations (named C3A1004E.Child here). These operations only
--     use entities declared in package Employees (named C3A1004E here),
--     but this package uses the limited view of a type imported from
--     package Departments (named C3A1004E here). We check that calls
--     to routines defined in Employees are allowed even if package
--     Departments is not withed.
--
-- SPECIAL REQUIREMENTS:
--      To build this test:
--         1) Do the steps required to add the limited view of the file
--            C3A10040 to the compilation environment. (Careful: the
--            compilation of the normal view of the file C3A10040 should
--            not be done at this time.)
--         2) Do the steps required to add the limited view of the file
--            C3A10041 to the environment.
--         3) Compile the file C3A10040 (and add the results to the
--            environment).
--         4) Compile the file C3A10041 (and add the results to the
--            environment).
--         5) Compile the file C3A10042 (and add the results to the
--            environment).
--         6) Build an executable image and run it.
--
-- TEST FILES:
--      This test consists of the following files:
--      -> C3A10040.A
--         C3A10041.A
--         C3A10042.AM
--
-- CHANGE HISTORY:
--    12 Jan 2015 RLB Created test.
--
--!
limited with C3A1004E;
package C3A1004D is -- Departments
   type Emp_Ptr is access all C3A1004E.Employee'Class;
   type Department is tagged private;

   procedure Choose_Manager
     (D       : in out Department;
      Manager :    out Emp_Ptr);

   procedure Create (D : in out Department; Name : in String);

   procedure Display (D : in Department);

   function Image (D : in Department) return String;

   procedure Appoint
    (E : in out C3A1004E.Employee'Class;
     D : in out Department);

   function Is_Member
    (E : in C3A1004E.Employee'Class;
     D : in Department) return Boolean;

private
   subtype Dept_Name is String (1 .. 13);
   type T_List is array (Positive range <>) of Emp_Ptr;

   type Department is tagged record
      Id   : Dept_Name := "<<Dept_Name>>";
      List : T_List (1 .. 5);
      Tot  : Natural := 0;
      During_Mod : Boolean := False;
   end record;
end C3A1004D;

