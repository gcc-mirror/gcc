-- CXB3019.A
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
--     Check that Program_Error is raised by the predefined equality operator
--     for an unchecked union type if either operand does not have inferable
--     discriminants.
--
--  TEST DESCRIPTION:
--     This checks B.3.3(23/2). A variety of combinations are tested (see below
--     for a complete list).
--
--     B.3.3(7/2) requires that all components of unchecked union types have
--     nominal subtypes that are C-compatible. To ensure this, we use types
--     from Interfaces.C and types with explicit convention C. Other types
--     could be C-compatible if the "implementation permits it" (B.1(20)), but
--     that is not suitable for an ACATS test.
--
--  LEGEND ------------------------+-------------------------------------------
--                                 |
--  CTE   - Compile-time error     |
--  Comp  - Component              |
--  Def   - Default                | Component : Unconstr_Rec_Type
--  Discr - Discriminated          |
--  Exp   - Explicit               | Component : Constr_Rec_Type (Value)
--  PE    - Program error          |
--  POC   - Per-object constrained | Component : Constr_Rec_Type (Discriminant)
--  Reg   - Regular                | Component : Integer
--  U_U   - Unchecked_Union        |
--  V     - Check, present         |
--  VP    - Variant part           |
--
--  TEST MATRIX  ----+
--                   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
--  Test number      | 1| 2| 3| 4| 5| 6| 7| 8| 9|10|11|12|13|14|15|16|17|18|19|
--                   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
--  Arg1 Constrained | -| V| -| V| -| V| -| V| -| V| -| V| -| V| -| V| -| V| -|
--  Arg2 Constrained | -| V| -| V| -| V| -| V| -| V| -| V| -| V| -| V| -| V| -|
--  Enclosing Discr  | V| V| V| V| V| V| V| V| V| V| V| V| V| V| V| V| V| V| V|
--  Enclosing U_U    | V| V| V| V| V| V| V| V| V| V| V| V| V| V| V| V| V| V| V|
--  Reg Comp         | V| V| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -|
--  Reg Def Comp     | -| -| V| V| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -|
--  Reg Exp Comp     | -| -| -| -| V| V| -| -| -| -| -| -| -| -| -| -| -| -| -|
--  Ref POC Comp     | -| -| -| -| -| -| V| V| -| -| -| -| -| -| -| -| -| -| -|
--  U_U Def Comp     | -| -| -| -| -| -| -| -| V| V| -| -| -| -| -| -| -| -| -|
--  U_U Exp Comp     | -| -| -| -| -| -| -| -| -| -| V| V| -| -| -| -| -| -| -|
--  U_U POC Comp     | -| -| -| -| -| -| -| -| -| -| -| -| V| V| -| -| -| -| -|
--  VP Present       | V| V| V| V| V| V| V| V| V| V| V| V| V| V| V| V| V| V| V|
--  VP Reg Comp      | V| V| V| V| V| V| V| V| V| V| V| V| V| V| -| -| -| -| -|
--  VP Reg Def Comp  | -| -| -| -| -| -| -| -| -| -| -| -| -| -| V| V| -| -| -|
--  VP Reg Exp Comp  | -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| V| V| -|
--  VP Reg POC Comp  | -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| V|
--  VP U_U Def Comp  | -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -|
--  VP U_U Exp Comp  | -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -|
--  VP U_U POC Comp  | -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -|
--  Raise CTE        | -| -| -| -| -| -| V| V| -| -| -| -| -| -| -| -| -| -| V|
--  Raise PE         | V| -| V| -| V| -| -| -| V| V| V| -| V| -| V| -| V| -| -|
---------------------+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
--  Test number      |20|21|22|23|24|25|26|
--                   +--+--+--+--+--+--+--+
--  Arg1 Constrained | V| -| V| -| V| -| V|
--  Arg2 Constrained | V| -| V| -| V| -| V|
--  Enclosing Discr  | V| V| V| V| V| V| V|
--  Enclosing U_U    | V| V| V| V| V| V| V|
--  Reg Comp         | V| V| V| V| V| V| V|
--  Reg Def Comp     | -| -| -| -| -| -| -|
--  Reg Exp Comp     | -| -| -| -| -| -| -|
--  Ref POC Comp     | -| -| -| -| -| -| -|
--  U_U Def Comp     | -| -| -| -| -| -| -|
--  U_U Exp Comp     | -| -| -| -| -| -| -|
--  U_U POC Comp     | -| -| -| -| -| -| -|
--  VP Present       | V| V| V| V| V| V| V|
--  VP Reg Comp      | -| -| -| -| -| -| -|
--  VP Reg Def Comp  | -| -| -| -| -| -| -|
--  VP Reg Exp Comp  | -| -| -| -| -| -| -|
--  VP Reg POC Comp  | V| -| -| -| -| -| -|
--  VP U_U Def Comp  | -| V| V| -| -| -| -|
--  VP U_U Exp Comp  | -| -| -| V| V| -| -|
--  VP U_U POC Comp  | -| -| -| -| -| V| V|
--  Raise CTE        | V| -| -| -| -| -| -|
--  Raise PE         | -| V| V| V| -| V| -|
---------------------+--+--+--+--+--+--+--+
--
--  APPLICABILITY CRITERIA:
--     This test is applicable to implementations that support package
--     Interfaces.C, aspect Convention C on record types, and aspect
--     Unchecked_Union.
--
--
--  CHANGE HISTORY:
--     29 JUL 2004  H K  Initial Version.
--     06 AUG 2004  H K  Modifications to handle lack of correct representation
--                       for Unchecked_Unions.
--     23 APR 2015  RLB  Modernized objective, ensured that all components
--                       of unions are C-compatible, added applicability
--                       criteria, removed compiler bug workaround.
--     13 JUL 2020  JAC  Actually check that an exception is raised.
--
--!

with Report; use Report;
with Interfaces.C; use Interfaces.C;                    -- N/A => ERROR.

procedure CXB3019 is

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

   --  All the tests are designed to equaluate to 'equal'

   procedure Test_EQ (Test_Name : String; Flag : Boolean) is
   begin
      if Flag then
         --Comment (Test_Name & " -     equal");
         null;
      else
         Failed (Test_Name & " - not equal");
      end if;
   end Test_EQ;

   procedure Unexpected (Test_Name : String) is
   begin
      Failed (Test_Name & " - Unexpected run-time error raised");
   end Unexpected;

   --  26 tests presenting different combinations of Unchecked_Union,
   --  non-Unchecked_Union components and per-object constraints.

   --  Must raise Program_Error since the actuals lack inferable discriminants
   function T001 return Boolean is
      type T2 (I : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         case I is                          --  Variant part
            when 0 =>
               Comp_T2_2 : Int := 2;
            when others =>
               Comp_T2_3 : Int := 3;        --  Regular
         end case;
      end record with Unchecked_Union;      -- N/A => ERROR.

      Obj1 : T2;                            --  Unconstrained
      Obj2 : T2;                            --  Unconstrained

   begin
      return Obj1 = Obj2;
   end T001;

   function T002 return Boolean is
      type T2 (I : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         case I is                          --  Variant part
            when 0 =>
               Comp_T2_2 : Int := 2;
            when others =>
               Comp_T2_3 : Int := 3;        --  Regular
         end case;
      end record with Unchecked_Union;

      Obj1 : T2 (4);                        --  Constrained
      Obj2 : T2 (5);                        --  Constrained
      Obj3 : T2 (Four);                     --  Constrained
      Obj4 : T2 (4);                        --  Constrained

   begin
      Obj4.Comp_T2_3 := 2;

      return Obj1 /= Obj2   --  not equal
               and then
             Obj1 = Obj3    --  equal
               and then
             Obj1 /= Obj4;  --  not equal
   end T002;

   --  Must raise Program_Error since the actuals lack inferable discriminants
   function T003 return Boolean is
      type T1 (I : Int := 0) is record
         case I is
            when 0 =>
               Comp_T1_1 : Int;
            when others =>
               Comp_T1_2 : Int;
         end case;
      end record with Convention => C;      -- N/A => ERROR.

      type T2 (D : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         Comp_T2_1 : T1;                    --  Default
         case D is                          --  Variant part
            when 0 =>
               Comp_T2_2 : Int := 2;
            when others =>
               Comp_T2_3 : Int := 3;        --  Regular
         end case;
      end record with Unchecked_Union;

      Obj1 : T2;                            --  Unconstrained
      Obj2 : T2;                            --  Unconstrained

   begin
      return Obj1 = Obj2;
   end T003;

   function T004 return Boolean is
      type T1 (I : Int := 0) is record
         case I is
            when 0 =>
               Comp_T1_1 : Int := 1;
            when others =>
               Comp_T1_2 : Int := 2;
         end case;
      end record with Convention => C;

      type T2 (D : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         Comp_T2_1 : T1;                    --  Default
         case D is                          --  Variant part
            when 0 =>
               Comp_T2_2 : Int := 2;
            when others =>
               Comp_T2_3 : Int := 3;        --  Regular
         end case;
      end record with Unchecked_Union;

      Obj1 : T2 (4);                        --  Constrained
      Obj2 : T2 (5);                        --  Constrained
      Obj3 : T2 (Four);                     --  Constrained
      Obj4 : T2 (4);                        --  Constrained
      Obj5 : T2 (4);                        --  Constrained

   begin
      Obj4.Comp_T2_1.Comp_T1_1 := 2;
      Obj5.Comp_T2_3 := 2;

      return Obj1 /= Obj2   --  not equal
               and then
             Obj1 = Obj3    --  equal
               and then
             Obj1 /= Obj4   --  not equal
               and then
             Obj1 /= Obj5;  --  not equal
   end T004;

   --  Must raise Program_Error since the actuals lack inferable discriminants
   function T005 return Boolean is
      type T1 (I : Int := 0) is record
         case I is
            when 0 =>
               Comp_T1_1 : Int;
            when others =>
               Comp_T1_2 : Int;
         end case;
      end record with Convention => C;

      type T2 (D : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         Comp_T2_1 : T1 (1);                --  Explicit
         case D is                          --  Variant part
            when 0 =>
               Comp_T2_2 : Int := 2;
            when others =>
               Comp_T2_3 : Int := 3;        --  Regular
         end case;
      end record with Unchecked_Union;

      Obj1 : T2;                            --  Unconstrained
      Obj2 : T2;                            --  Unconstrained

   begin
      return Obj1 = Obj2;
   end T005;

   function T006 return Boolean is
      type T1 (I : Int := 0) is record
         case I is
            when 0 =>
               Comp_T1_1 : Int := 1;
            when others =>
               Comp_T1_2 : Int := 2;
         end case;
      end record with Convention => C;

      type T2 (D : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         Comp_T2_1 : T1 (1);                --  Explicit
         case D is                          --  Variant part
            when 0 =>
               Comp_T2_2 : Int := 2;
            when others =>
               Comp_T2_3 : Int := 3;        --  Regular
         end case;
      end record with Unchecked_Union;

      Obj1 : T2 (4);                        --  Constrained
      Obj2 : T2 (5);                        --  Constrained
      Obj3 : T2 (Four);                     --  Constrained
      Obj4 : T2 (4);                        --  Constrained
      Obj5 : T2 (4);                        --  Constrained

   begin
      Obj4.Comp_T2_1.Comp_T1_2 := 1;
      Obj5.Comp_T2_3 := 2;

      return Obj1 /= Obj2   --  not equal
               and then
             Obj1 = Obj3    --  equal
               and then
             Obj1 /= Obj4   --  not equal
               and then
             Obj1 /= Obj5;  --  not equal
   end T006;

   --  Must raise Program_Error since:
   --  1. The actuals lack inferable discriminants
   --  2. Comp_T2_1 lacks inferable discriminants
   function T009 return Boolean is
      type T1 (I : Int := 0) is record
         case I is
            when 0 =>
               Comp_T1_1 : Int;
            when others =>
               Comp_T1_2 : Int;
         end case;
      end record with Unchecked_Union, Convention => C;

      type T2 (D : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         Comp_T2_1 : T1;                    --  Default
         case D is                          --  Variant part
            when 0 =>
               Comp_T2_2 : Int := 2;
            when others =>
               Comp_T2_3 : Int := 3;        --  Regular
         end case;
      end record with Unchecked_Union;

      Obj1 : T2;                            --  Unconstrained
      Obj2 : T2;                            --  Unconstrained

   begin
      return Obj1 = Obj2;
   end T009;

   --  Must raise Program_Error since Comp_T2_1 lacks inferable discriminants
   function T010 return Boolean is
      type T1 (I : Int := 0) is record
         case I is
            when 0 =>
               Comp_T1_1 : Int := 1;
            when others =>
               Comp_T1_2 : Int := 2;
         end case;
      end record with Unchecked_Union, Convention => C;

      type T2 (D : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         Comp_T2_1 : T1;                    --  Default
         case D is                          --  Variant part
            when 0 =>
               Comp_T2_2 : Int := 2;
            when others =>
               Comp_T2_3 : Int := 3;        --  Regular
         end case;
      end record with Unchecked_Union;

      Obj1 : T2 (4);                        --  Constrained
      Obj2 : T2 (5);                        --  Constrained
      Obj3 : T2 (Four);                     --  Constrained
      Obj4 : T2 (4);                        --  Constrained
      Obj5 : T2 (4);                        --  Constrained

   begin
      Obj4.Comp_T2_1.Comp_T1_1 := 2;
      Obj5.Comp_T2_3 := 2;

      return Obj1 /= Obj2   --  not equal
               and then
             Obj1 = Obj3    --  equal
               and then
             Obj1 /= Obj4   --  not equal
               and then
             Obj1 /= Obj5;  --  not equal
   end T010;

   --  Must raise Program_Error since the actuals lack inferable discriminants
   function T011 return Boolean is
      type T1 (I : Int := 0) is record
         case I is
            when 0 =>
               Comp_T1_1 : Int;
            when others =>
               Comp_T1_2 : Int;
         end case;
      end record with Unchecked_Union, Convention => C;

      type T2 (D : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         Comp_T2_1 : T1 (1);                --  Explicit
         case D is                          --  Variant part
            when 0 =>
               Comp_T2_2 : Int := 2;
            when others =>
               Comp_T2_3 : Int := 3;        --  Regular
         end case;
      end record with Unchecked_Union;

      Obj1 : T2;                            --  Unconstrained
      Obj2 : T2;                            --  Unconstrained

   begin
      return Obj1 = Obj2;
   end T011;

   function T012 return Boolean is
      type T1 (I : Int := 0) is record
         case I is
            when 0 =>
               Comp_T1_1 : Int := 1;
            when others =>
               Comp_T1_2 : Int := 2;
         end case;
      end record with Unchecked_Union, Convention => C;

      type T2 (D : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         Comp_T2_1 : T1 (1);                --  Explicit
         case D is                          --  Variant part
            when 0 =>
               Comp_T2_2 : Int := 2;
            when others =>
               Comp_T2_3 : Int := 3;        --  Regular
         end case;
      end record with Unchecked_Union;

      Obj1 : T2 (4);                        --  Constrained
      Obj2 : T2 (5);                        --  Constrained
      Obj3 : T2 (Four);                     --  Constrained
      Obj4 : T2 (4);                        --  Constrained
      Obj5 : T2 (4);                        --  Constrained

   begin
      Obj4.Comp_T2_1.Comp_T1_2 := 1;
      Obj5.Comp_T2_3 := 2;

      return Obj1 /= Obj2   --  not equal
               and then
             Obj1 = Obj3    --  equal
               and then
             Obj1 /= Obj4   --  not equal
               and then
             Obj1 /= Obj5;  --  not equal
   end T012;

   --  Must raise Program_Error since the actuals lack inferable discriminants
   function T013 return Boolean is
      type T1 (I : Int := 0) is record
         case I is
            when 0 =>
               Comp_T1_1 : Int;
            when others =>
               Comp_T1_2 : Int;
         end case;
      end record with Unchecked_Union, Convention => C;

      type T2 (D : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         Comp_T2_1 : T1 (D);                --  POC
         case D is                          --  Variant part
            when 0 =>
               Comp_T2_2 : Int := 2;
            when others =>
               Comp_T2_3 : Int := 3;        --  Regular
         end case;
      end record with Unchecked_Union;

      Obj1 : T2;                            --  Unconstrained
      Obj2 : T2;                            --  Unconstrained

   begin
      return Obj1 = Obj2;
   end T013;

   function T014 return Boolean is
      type T1 (I : Int := 0) is record
         case I is
            when 0 =>
               Comp_T1_1 : Int := 1;
            when others =>
               Comp_T1_2 : Int := 2;
         end case;
      end record with Unchecked_Union, Convention => C;

      type T2 (D : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         Comp_T2_1 : T1 (D);                --  POC
         case D is                          --  Variant part
            when 0 =>
               Comp_T2_2 : Int := 2;
            when others =>
               Comp_T2_3 : Int := 3;        --  Regular
         end case;
      end record with Unchecked_Union;

      Obj1 : T2 (4);                        --  Constrained
      Obj2 : T2 (5);                        --  Constrained
      Obj3 : T2 (Four);                     --  Constrained
      Obj4 : T2 (4);                        --  Constrained
      Obj5 : T2 (4);                        --  Constrained

   begin
      Obj4.Comp_T2_1.Comp_T1_2 := 1;
      Obj5.Comp_T2_3 := 2;

      return Obj1 /= Obj2   --  not equal
               and then
             Obj1 = Obj3    --  equal
               and then
             Obj1 /= Obj4   --  not equal
               and then
             Obj1 /= Obj5;  --  not equal
   end T014;

   --  Must raise Program_Error since the actuals lack inferable discriminants
   function T015 return Boolean is
      type T1 (I : Int := 0) is record
         case I is
            when 0 =>
               Comp_T1_1 : Int;
            when others =>
               Comp_T1_2 : Int;
         end case;
      end record with Convention => C;

      type T2 (D : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         case D is                          --  Variant part
            when 0 =>
               Comp_T2_2 : Int := 2;
            when others =>
               Comp_T2_3 : T1;              --  Default
         end case;
      end record with Unchecked_Union;

      Obj1 : T2;                            --  Unconstrained
      Obj2 : T2;                            --  Unconstrained

   begin
      return Obj1 = Obj2;
   end T015;

   function T016 return Boolean is
      type T1 (I : Int := 0) is record
         case I is
            when 0 =>
               Comp_T1_1 : Int := 1;
            when others =>
               Comp_T1_2 : Int := 2;
         end case;
      end record with Convention => C;

      type T2 (D : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         case D is                          --  Variant part
            when 0 =>
               Comp_T2_2 : Int := 2;
            when others =>
               Comp_T2_3 : T1;              --  Default
         end case;
      end record with Unchecked_Union;

      Obj1 : T2 (4);                        --  Constrained
      Obj2 : T2 (5);                        --  Constrained
      Obj3 : T2 (Four);                     --  Constrained
      Obj4 : T2 (4);                        --  Constrained
      Obj5 : T2 (4);                        --  Constrained

   begin
      Obj4.Comp_T2_3.Comp_T1_1 := 2;
      Obj5.Comp_T2_0 := 2;

      return Obj1 /= Obj2   --  not equal
               and then
             Obj1 = Obj3    --  equal
               and then
             Obj1 /= Obj4   --  not equal
               and then
             Obj1 /= Obj5;  --  not equal
   end T016;

   --  Must raise Program_Error since the actuals lack inferable discriminants
   function T017 return Boolean is
      type T1 (I : Int := 0) is record
         case I is
            when 0 =>
               Comp_T1_1 : Int;
            when others =>
               Comp_T1_2 : Int;
         end case;
      end record with Convention => C;

      type T2 (D : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         case D is                          --  Variant part
            when 0 =>
               Comp_T2_2 : Int := 2;
            when others =>
               Comp_T2_3 : T1 (3);          --  Explicit
         end case;
      end record with Unchecked_Union;

      Obj1 : T2;                            --  Unconstrained
      Obj2 : T2;                            --  Unconstrained

   begin
      return Obj1 = Obj2;
   end T017;

   function T018 return Boolean is
      type T1 (I : Int := 0) is record
         case I is
            when 0 =>
               Comp_T1_1 : Int := 1;
            when others =>
               Comp_T1_2 : Int := 2;
         end case;
      end record with Convention => C;

      type T2 (D : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         case D is                          --  Variant part
            when 0 =>
               Comp_T2_2 : Int := 2;
            when others =>
               Comp_T2_3 : T1 (3);          --  Explicit
         end case;
      end record with Unchecked_Union;

      Obj1 : T2 (4);                        --  Constrained
      Obj2 : T2 (5);                        --  Constrained
      Obj3 : T2 (Four);                     --  Constrained
      Obj4 : T2 (4);                        --  Constrained
      Obj5 : T2 (4);                        --  Constrained

   begin
      Obj4.Comp_T2_3.Comp_T1_2 := 1;
      Obj5.Comp_T2_0 := 2;

      return Obj1 /= Obj2   --  not equal
               and then
             Obj1 = Obj3    --  equal
               and then
             Obj1 /= Obj4   --  not equal
               and then
             Obj1 /= Obj5;  --  not equal
   end T018;

   --  Must raise Program_Error since:
   --  1. The actuals lack inferable discriminants
   --  2. Comp_T2_3 lacks inferable discriminants
   function T021 return Boolean is
      type T1 (I : Int := 0) is record
         case I is
            when 0 =>
               Comp_T1_1 : Int;
            when others =>
               Comp_T1_2 : Int;
         end case;
      end record with Unchecked_Union, Convention => C;

      type T2 (D : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         case D is                          --  Variant part
            when 0 =>
               Comp_T2_2 : Int := 2;
            when others =>
               Comp_T2_3 : T1;              --  Default
         end case;
      end record with Unchecked_Union;

      Obj1 : T2;                            --  Unconstrained
      Obj2 : T2;                            --  Unconstrained

   begin
      return Obj1 = Obj2;
   end T021;

   --  Must raise Program_Error since Comp_T2_3 lacks inferable discriminants
   function T022 return Boolean is
      type T1 (I : Int := 0) is record
         case I is
            when 0 =>
               Comp_T1_1 : Int := 1;
            when others =>
               Comp_T1_2 : Int := 2;
         end case;
      end record with Unchecked_Union, Convention => C;

      type T2 (D : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         case D is                          --  Variant part
            when 0 =>
               Comp_T2_2 : Int := 2;
            when others =>
               Comp_T2_3 : T1;              --  Default
         end case;
      end record with Unchecked_Union;

      Obj1 : T2 (4);                        --  Constrained
      Obj2 : T2 (5);                        --  Constrained
      Obj3 : T2 (Four);                     --  Constrained
      Obj4 : T2 (4);                        --  Constrained
      Obj5 : T2 (4);                        --  Constrained

   begin
      Obj4.Comp_T2_3.Comp_T1_1 := 2;
      Obj5.Comp_T2_0 := 2;

      return Obj1 /= Obj2   --  not equal
               and then
             Obj1 = Obj3    --  equal
               and then
             Obj1 /= Obj4   --  not equal
               and then
             Obj1 /= Obj5;  --  not equal
   end T022;

   --  Must raise Program_Error since the actuals lack inferable discriminants
   function T023 return Boolean is
      type T1 (I : Int := 0) is record
         case I is
            when 0 =>
               Comp_T1_1 : Int;
            when others =>
               Comp_T1_2 : Int;
         end case;
      end record with Unchecked_Union, Convention => C;

      type T2 (D : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         case D is                          --  Variant part
            when 0 =>
               Comp_T2_2 : Int := 2;
            when others =>
               Comp_T2_3 : T1 (3);          --  Explicit
         end case;
      end record with Unchecked_Union;

      Obj1 : T2;                            --  Unconstrained
      Obj2 : T2;                            --  Unconstrained

   begin
      return Obj1 = Obj2;
   end T023;

   function T024 return Boolean is
      type T1 (I : Int := 0) is record
         case I is
            when 0 =>
               Comp_T1_1 : Int := 1;
            when others =>
               Comp_T1_2 : Int := 2;
         end case;
      end record with Unchecked_Union, Convention => C;

      type T2 (D : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         case D is                          --  Variant part
            when 0 =>
               Comp_T2_2 : Int := 2;
            when others =>
               Comp_T2_3 : T1 (3);          --  Explicit
         end case;
      end record with Unchecked_Union;

      Obj1 : T2 (4);                        --  Constrained
      Obj2 : T2 (5);                        --  Constrained
      Obj3 : T2 (Four);                     --  Constrained
      Obj4 : T2 (4);                        --  Constrained
      Obj5 : T2 (4);                        --  Constrained

   begin
      Obj4.Comp_T2_3.Comp_T1_2 := 0;
      Obj5.Comp_T2_0 := 3;

      return Obj1 /= Obj2   --  not equal
               and then
             Obj1 = Obj3    --  equal
               and then
             Obj1 /= Obj4   --  not equal
               and then
             Obj1 /= Obj5;  --  not equal
   end T024;

   --  Must raise Program_Error since the actuals lack inferable discriminants
   function T025 return Boolean is
      type T1 (I : Int := 0) is record
         case I is
            when 0 =>
               Comp_T1_1 : Int;
            when others =>
               Comp_T1_2 : Int;
         end case;
      end record with Unchecked_Union, Convention => C;

      type T2 (D : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         case D is                          --  Variant part
            when 0 =>
               Comp_T2_2 : Int := 2;
            when others =>
               Comp_T2_3 : T1 (D);          --  POC
         end case;
      end record with Unchecked_Union;

      Obj1 : T2;                            --  Unconstrained
      Obj2 : T2;                            --  Unconstrained

   begin
      return Obj1 = Obj2;
   end T025;

   function T026 return Boolean is
      type T1 (I : Int := 0) is record
         case I is
            when 0 =>
               Comp_T1_1 : Int := 1;
            when others =>
               Comp_T1_2 : Int := 2;
         end case;
      end record with Unchecked_Union, Convention => C;

      type T2 (D : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         case D is                          --  Variant part
            when 0 =>
               Comp_T2_2 : Int := 2;
            when others =>
               Comp_T2_3 : T1 (D);          --  POC
         end case;
      end record with Unchecked_Union;

      Obj1 : T2 (4);                        --  Constrained
      Obj2 : T2 (5);                        --  Constrained
      Obj3 : T2 (Four);                     --  Constrained
      Obj4 : T2 (4);                        --  Constrained
      Obj5 : T2 (4);                        --  Constrained

   begin
      Obj4.Comp_T2_3.Comp_T1_2 := 1;
      Obj5.Comp_T2_0 := 3;

      return Obj1 /= Obj2   --  not equal
               and then
             Obj1 = Obj3    --  equal
               and then
             Obj1 /= Obj4   --  not equal
               and then
             Obj1 /= Obj5;  --  not equal
   end T026;

begin
   Test ("CXB3019", "Check that Program_Error is raised by the predefined " &
                    "equality operator for an unchecked union type if " &
                    "either operand does not have inferable discriminants");

   begin
      Test_EQ ("T001", T001);
      Failed ("T001 Exception not raised");
   exception
      when Program_Error =>
         Correct_PE ("T001");
      when others =>
         Unexpected ("T001");
   end;

   begin
      Test_EQ ("T002", T002);
   exception
      when Program_Error =>
         Incorrect_PE ("T002");
      when others =>
         Unexpected ("T002");
   end;

   begin
      Test_EQ ("T003", T003);
      Failed ("T003 Exception not raised");
   exception
      when Program_Error =>
         Correct_PE ("T003");
      when others =>
         Unexpected ("T003");
   end;

   begin
      Test_EQ ("T004", T004);
   exception
      when Program_Error =>
         Incorrect_PE ("T004");
      when others =>
         Unexpected ("T004");
   end;

   begin
      Test_EQ ("T005", T005);
      Failed ("T005 Exception not raised");
    exception
      when Program_Error =>
         Correct_PE ("T005");
      when others =>
         Unexpected ("T005");
   end;

   begin
      Test_EQ ("T006", T006);
   exception
      when Program_Error =>
         Incorrect_PE ("T006");
      when others =>
         Unexpected ("T006");
   end;

   --Comment ("T007 - Purposely missing"); -- Type would be illegal.

   --Comment ("T008 - Purposely missing"); -- Type would be illegal.

   begin
      Test_EQ ("T009", T009);
      Failed ("T009 Exception not raised");
   exception
      when Program_Error =>
         Correct_PE ("T009");
      when others =>
         Unexpected ("T009");
   end;

   begin
      Test_EQ ("T010", T010);
      Failed ("T010 Exception not raised");
   exception
      when Program_Error =>
         Correct_PE ("T010");
      when others =>
         Unexpected ("T010");
   end;

   begin
      Test_EQ ("T011", T011);
      Failed ("T011 Exception not raised");
   exception
      when Program_Error =>
         Correct_PE ("T011");
      when others =>
         Unexpected ("T011");
   end;

   begin
      Test_EQ ("T012", T012);
   exception
      when Program_Error =>
         Incorrect_PE ("T012");
      when others =>
         Unexpected ("T012");
   end;

   begin
      Test_EQ ("T013", T013);
      Failed ("T013 Exception not raised");
   exception
      when Program_Error =>
         Correct_PE ("T013");
      when others =>
         Unexpected ("T013");
   end;

   begin
      Test_EQ ("T014", T014);
   exception
      when Program_Error =>
         Incorrect_PE ("T014");
      when others =>
         Unexpected ("T014");
   end;

   begin
      Test_EQ ("T015", T015);
      Failed ("T015 Exception not raised");
   exception
      when Program_Error =>
         Correct_PE ("T015");
      when others =>
         Unexpected ("T015");
   end;

   begin
      Test_EQ ("T016", T016);
   exception
      when Program_Error =>
         Incorrect_PE ("T016");
      when others =>
         Unexpected ("T016");
   end;

   begin
      Test_EQ ("T017", T017);
      Failed ("T017 Exception not raised");
   exception
      when Program_Error =>
         Correct_PE ("T017");
      when others =>
         Unexpected ("T017");
   end;

   begin
      Test_EQ ("T018", T018);
   exception
      when Program_Error =>
         Incorrect_PE ("T018");
      when others =>
         Unexpected ("T018");
   end;

   --Comment ("T019 - Purposely missing"); -- Type would be illegal.

   --Comment ("T020 - Purposely missing"); -- Type would be illegal.

   begin
      Test_EQ ("T021", T021);
      Failed ("T021 Exception not raised");
   exception
      when Program_Error =>
         Correct_PE ("T021");
      when others =>
         Unexpected ("T021");
   end;

   begin
      Test_EQ ("T022", T022);
      Failed ("T022 Exception not raised");
   exception
      when Program_Error =>
         Correct_PE ("T022");
      when others =>
         Unexpected ("T022");
   end;

   begin
      Test_EQ ("T023", T023);
      Failed ("T023 Exception not raised");
   exception
      when Program_Error =>
         Correct_PE ("T023");
      when others =>
         Unexpected ("T023");
   end;

   begin
      Test_EQ ("T024", T024);
   exception
      when Program_Error =>
         Incorrect_PE ("T024");
      when others =>
         Unexpected ("T024");
   end;

   begin
      Test_EQ ("T025", T025);
      Failed ("T025 Exception not raised");
   exception
      when Program_Error =>
         Correct_PE ("T025");
      when others =>
         Unexpected ("T025");
   end;

   begin
      Test_EQ ("T026", T026);
   exception
      when Program_Error =>
         Incorrect_PE ("T026");
      when others =>
         Unexpected ("T026");
   end;

   Result;
end CXB3019;
