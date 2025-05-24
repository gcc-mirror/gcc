-- CXB3021.A
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
--      Check that Program_Error is raised by a membership test if a
--      subtype_mark denotes a constrained unchecked union subtype and the
--      expression lacks inferable discriminants.
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

procedure CXB3021 is

   --  A couple of useful subprograms

   procedure Correct_PE (Test_Name : String) is
   begin
      --Comment (Test_Name & " - Program_Error was correctly raised");
      null;
   end Correct_PE;

   function Four return Int is
   begin
      return Int(Report.Ident_Int(4));
   end Four;

   procedure Incorrect_PE (Test_Name : String) is
   begin
      Failed (Test_Name & " - Program_Error was INCORRECTLY raised");
   end Incorrect_PE;

   procedure Test_IN (Test_Name : String; Flag : Boolean) is
   begin
      if Flag then
         --Comment (Test_Name & " -     in");
         null;
      else
         Failed (Test_Name & " - not not");
      end if;
   end Test_IN;

   procedure Unexpected (Test_Name : String) is
   begin
      Failed (Test_Name & " - Unexpected run-time error raised");
   end Unexpected;

   --  An Unchecked_Union type and two subtypes

   type T1 (D : Int := 0) is record
      Comp_T1_1 : Int;
      case D is
         when 0 =>
            Comp_T1_2 : Int;
         when others =>
            Comp_T1_3 : Int;
      end case;
   end record with Unchecked_Union;

   subtype ST1 is T1;
   subtype ST2 is T1 (1);

   --  Must raise Program_Error since Obj_T1_1 lacks inferable discriminants
   function T001 return Boolean is
      Obj_T1_1  : T1;

   begin
      return Obj_T1_1 in ST2;
   end T001;

   --  Must raise Program_Error since Obj_ST1_1 lacks inferable discriminants
   function T002 return Boolean is
      Obj_ST1_1 : ST1;

   begin
      return Obj_ST1_1 in ST2;
   end T002;

   function T003 return Boolean is
      Obj_T1_1 : T1;

   begin
      return Obj_T1_1 in T1;
   end T003;

   function T004 return Boolean is
      Obj_T1_1 : ST1;

   begin
      return Obj_T1_1 in ST1;
   end T004;

   function T005 return Boolean is
      Obj_T1_2 : T1 (Four);

   begin
      return Obj_T1_2 in T1;
   end T005;

   function T006 return Boolean is
      Obj_T1_2 : T1 (Four);

   begin
      return Obj_T1_2 in ST1;
   end T006;

   function T007 return Boolean is
      Obj_ST1_1 : ST1;

   begin
      return Obj_ST1_1 in T1;
   end T007;

   function T008 return Boolean is
      Obj_ST1_1 : ST1;

   begin
      return Obj_ST1_1 in ST1;
   end T008;

   function T009 return Boolean is
      Obj_ST2_1 : ST2;

   begin
      return Obj_ST2_1 in T1;
   end T009;

   function T010 return Boolean is
      Obj_ST2_1 : ST2;

   begin
      return Obj_ST2_1 in ST1;
   end T010;

   function T011 return Boolean is
      Obj_T1_2 : T1 (Four);

   begin
      return Obj_T1_2 in ST2;
   end T011;

   function T012 return Boolean is
      Obj_ST2_1 : ST2;

   begin
      return Obj_ST2_1 in ST2;
   end T012;

begin
   Test ("CXB3021", "Check that Program_Error is raised by a membership " &
                     "test if a subtype_mark denotes a constrained " &
                     "unchecked union subtype and the expression lacks " &
                     "inferable discriminants");

   begin
      Test_IN ("T001", T001);
      Failed ("T001 Exception not raised");
   exception
      when Program_Error =>
         Correct_PE ("T001");
      when others =>
         Unexpected ("T001");
   end;

   begin
      Test_IN ("T002", T002);
      Failed ("T002 Exception not raised");
   exception
      when Program_Error =>
         Correct_PE ("T002");
      when others =>
         Unexpected ("T002");
   end;

   begin
      Test_IN ("T003", T003);
   exception
      when Program_Error =>
         Incorrect_PE ("T003");
      when others =>
         Unexpected ("T003");
   end;

   begin
      Test_IN ("T004", T004);
   exception
      when Program_Error =>
         Incorrect_PE ("T004");
      when others =>
         Unexpected ("T004");
   end;

   begin
      Test_IN ("T005", T005);
    exception
      when Program_Error =>
         Incorrect_PE ("T005");
      when others =>
         Unexpected ("T005");
   end;

   begin
      Test_IN ("T006", T006);
   exception
      when Program_Error =>
         Incorrect_PE ("T006");
      when others =>
         Unexpected ("T006");
   end;

   begin
      Test_IN ("T007", T007);
   exception
      when Program_Error =>
         Incorrect_PE ("T007");
      when others =>
         Unexpected ("T007");
   end;

   begin
      Test_IN ("T008", T008);
   exception
      when Program_Error =>
         Incorrect_PE ("T008");
      when others =>
         Unexpected ("T008");
   end;

   begin
      Test_IN ("T009", T009);
   exception
      when Program_Error =>
         Incorrect_PE ("T009");
      when others =>
         Unexpected ("T009");
   end;

   begin
      Test_IN ("T010", T010);
   exception
      when Program_Error =>
         Incorrect_PE ("T010");
      when others =>
         Unexpected ("T010");
   end;

   begin
      Test_IN ("T011", T011);
   exception
      when Program_Error =>
         Incorrect_PE ("T011");
      when others =>
         Unexpected ("T011");
   end;

   begin
      Test_IN ("T012", T012);
   exception
      when Program_Error =>
         Incorrect_PE ("T012");
      when others =>
         Unexpected ("T012");
   end;

   Result;
end CXB3021;
