-- CXB3022.A
--
--                             Grant of Unlimited Rights
--
--     Ada Core Technologies Inc. (AdaCore) holds unlimited rights in the
--     software and documentation contained herein. Unlimited rights are
--     the same as those granted by the U.S. Government for older parts of
--     the Ada Conformity Assessment Test Suite, and are defined in DFAR
--     252.227-7013(a)(19). By making this public release, AdaCore intends
--     to confer upon all recipients unlimited rights equal to those held by
--     the Ada Conformity Assessment Authority. These rights include rights
--     to use, duplicate, release or disclose the released technical data and
--     computer software in whole or in part, in any manner and for any purpose
--     whatsoever, and to have or permit others to do so.
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
--     This test is based on one submitted by AdaCore; AdaCore retains
--     the copyright on the test.
--
--*
--  OBJECTIVE:
--     Check that Program_Error is raised by the conversion from a derived
--     unchecked union type to an unconstrained non-unchecked-union type if
--     the operand of the conversion lacks inferable discriminants.
--
--  TEST DESCRIPTION:
--     This checks B.3.3(25/2). A variety of combinations are tested.
--
--     B.3.3(7/2) requires that all components of unchecked union types have
--     nominal subtypes that are C-compatible. To ensure this, we use types
--     from Interfaces.C; and we could have used types with explicit
--     convention C. Other types could be C-compatible if the "implementation
--     permits it" (B.1(20)), but that is not suitable for an ACATS test.
--
--  APPLICABILITY CRITERIA:
--     This test is applicable to implementations that support package
--     Interfaces.C and aspect Unchecked_Union.
--
--  CHANGE HISTORY:
--     30 JUL 2004  H K  Initial Version.
--     23 APR 2015  RLB  Modernized objective, ensured that all components
--                       of unions are C-compatible, added applicability
--                       criteria, removed compiler bug workaround.
--     13 JUL 2020  JAC  Actually check that an exception is raised.
--!
with Report; use Report;
with Interfaces.C; use Interfaces.C;                    -- N/A => ERROR.

procedure CXB3022 is

   --  A couple of useful subprograms

   procedure Correct_PE (Test_Name : String) is
   begin
      --Comment (Test_Name & " - Program_Error was correctly raised");
      null;
   end Correct_PE;

   function Four return Integer is
   begin
      return 4;
   end Four;

   procedure Incorrect_PE (Test_Name : String) is
   begin
      Failed (Test_Name & " - Program_Error was INCORRECTLY raised");
   end Incorrect_PE;

   procedure Unexpected (Test_Name : String) is
   begin
      Failed (Test_Name & " - Unexpected run-time error raised");
   end Unexpected;

   --  An discriminated record type and a derived Unchecked_Union type

   type Rec_Typ (I : Int := 0) is record
      Comp_0 : Int := 0;
      Comp_1 : Int := 1;
      case I is
         when 0 =>
            Comp_2 : Int := 2;
         when 1 =>
            Comp_3 : Int;
         when 2 =>
            Comp_4 : Int := 4;
         when 3 =>
            Comp_5 : Int;
         when others =>
            Comp_Other : Int;
      end case;
   end record;

   type Derived_U_U is new Rec_Typ with Unchecked_Union;

   --  Must raise Program_Error since Obj_DU_1 lacks inferable discriminants
   procedure T001 is
      Obj_RT_1 : Rec_Typ;
      Obj_DU_1 : Derived_U_U;

   begin
      Obj_RT_1 := Rec_Typ (Obj_DU_1);
   end T001;

   procedure T002 is
      Obj_RT_1 : Rec_Typ;
      Obj_DU_1 : Derived_U_U (1);

   begin
      Obj_RT_1 := Rec_Typ (Obj_DU_1);
   end T002;

   procedure T003 is
      Obj_RT_1 : Rec_Typ (1);
      Obj_DU_1 : Derived_U_U (1);

   begin
      Obj_RT_1 := Rec_Typ (Obj_DU_1);
   end T003;

begin
   Test ("CXB3022", "Check that Program_Error is raised by the conversion " &
                    "from a derived unchecked union type to an " &
                    "unconstrained non-unchecked-union type if the operand " &
                    "of the conversion lacks inferable discriminants");

   begin
      T001;
      Failed ("T001 Exception not raised");
   exception
      when Program_Error =>
         Correct_PE ("T001");
      when others =>
         Unexpected ("T001");
   end;

   begin
      T002;
      --Comment ("T002 - Conversion complete");
   exception
      when Program_Error =>
         Incorrect_PE ("T002");
      when others =>
         Unexpected ("T002");
   end;

   begin
      T003;
      --Comment ("T003 - Conversion complete");
   exception
      when Program_Error =>
         Incorrect_PE ("T003");
      when others =>
         Unexpected ("T003");
   end;

   Result;
end CXB3022;
