-- CXB3020.A
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
--     for any type that has a subcomponent of an unchecked union type whose
--     nominal subtype is unconstrained.
--
--  TEST DESCRIPTION:
--     This checks B.3.3(24/2). A variety of combinations are tested (see
--     below).
--
--     B.3.3(7/2) requires that all components of unchecked union types have
--     nominal subtypes that are C-compatible. To ensure this, we use types
--     from Interfaces.C; and we could have used types with explicit
--     convention C. Other types could be C-compatible if the "implementation
--     permits it" (B.1(20)), but that is not suitable for an ACATS test.
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
--  Arg1 Constrained | -| -| V| -| V| -| V| -| V| -| V| -| V| -| V| -| V| -| V|
--  Arg2 Constrained | -| -| -| -| -| -| -| -| V| -| V| -| V| -| -| -| -| -| -|
--  Enclosing Discr  | -| V| V| V| V| V| V| V| V| V| V| V| V| V| V| V| V| V| V|
--  Enclosing U_U    | -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -|
--  Reg Comp         | V| V| V| V| V| V| V| V| V| V| V| V| V| V| V| V| V| V| V|
--  Reg Def Comp     | -| V| V| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -|
--  Reg Exp Comp     | -| -| -| V| V| -| -| -| -| -| -| -| -| -| -| -| -| -| -|
--  Ref POC Comp     | -| -| -| -| -| V| V| -| -| -| -| -| -| -| -| -| -| -| -|
--  U_U Def Comp     | -| -| -| -| -| -| -| V| V| -| -| -| -| -| -| -| -| -| -|
--  U_U Exp Comp     | -| -| -| -| -| -| -| -| -| V| V| -| -| -| -| -| -| -| -|
--  U_U POC Comp     | -| -| -| -| -| -| -| -| -| -| -| V| V| -| -| -| -| -| -|
--  VP Present       | -| -| -| -| -| -| -| -| -| -| -| -| -| V| V| V| V| V| V|
--  VP Reg Comp      | -| -| -| -| -| -| -| -| -| -| -| -| -| V| V| -| -| -| -|
--  VP Reg Def Comp  | -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| V| V| -| -|
--  VP Reg Exp Comp  | -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| V| V|
--  VP Red POC Comp  | -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -|
--  VP U_U Def Comp  | -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -|
--  VP U_U Exp Comp  | -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -|
--  VP U_U POC Comp  | -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -|
--  Raise CTE        | -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -| -|
--  Raise PE         | -| -| -| -| -| -| -| V| V| -| -| V| -| -| -| -| -| -| -|
---------------------+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
--  Test number      |20|21|22|23|24|25|26|27|
--                   +--+--+--+--+--+--+--+--+
--  Arg1 Constrained | -| V| -| V| -| V| -| V|
--  Arg2 Constrained | -| -| -| V| -| V| -| V|
--  Enclosing Discr  | V| V| V| V| V| V| V| V|
--  Enclosing U_U    | -| -| -| -| -| -| -| -|
--  Reg Comp         | V| V| V| V| V| V| V| V|
--  Reg Def Comp     | -| -| -| -| -| -| -| -|
--  Reg Exp Comp     | -| -| -| -| -| -| -| -|
--  Ref POC Comp     | -| -| -| -| -| -| -| -|
--  U_U Def Comp     | -| -| -| -| -| -| -| -|
--  U_U Exp Comp     | -| -| -| -| -| -| -| -|
--  U_U POC Comp     | -| -| -| -| -| -| -| -|
--  VP Present       | V| V| V| V| V| V| V| V|
--  VP Reg Comp      | -| -| -| -| -| -| -| -|
--  VP Reg Def Comp  | -| -| -| -| -| -| -| -|
--  VP Reg Exp Comp  | -| -| -| -| -| -| -| -|
--  VP Red POC Comp  | V| V| -| -| -| -| -| -|
--  VP U_U Def Comp  | -| -| V| V| -| -| -| -|
--  VP U_U Exp Comp  | -| -| -| -| V| V| -| -|
--  VP U_U POC Comp  | -| -| -| -| -| -| V| V|
--  Raise CTE        | -| -| -| -| -| -| -| -|
--  Raise PE         | -| -| V| V| -| -| -| -|
---------------------+--+--+--+--+--+--+--+--+
--
--  APPLICABILITY CRITERIA:
--     This test is applicable to implementations that support package
--     Interfaces.C and aspect Unchecked_Union.
--
--  CHANGE HISTORY:
--     29 JUL 2004  H K  Initial Version.
--     06 AUG 2004  H K  Include more equality comparisons where
--                       Unchecked_Unions are present. Modifications to handle
--                       the lack of correct representation for
--                       Unchecked_Unions.
--     23 APR 2015  RLB  Modernized objective, ensured that all components
--                       of unions are C-compatible, added applicability
--                       criteria, removed compiler bug workaround.
--     13 JUL 2020  JAC  Actually check that an exception is raised.
--!

with Report; use Report;
with Interfaces.C; use Interfaces.C;                    -- N/A => ERROR.
with Ada.Exceptions;

procedure CXB3020 is

   procedure Correct_PE (Test_Name : String) is
   begin
      --Comment (Test_Name & " - Program_Error was correctly raised");
      null;
   end Correct_PE;

   function Four return Int is
   begin
      return 4;
   end Four;

   procedure Incorrect_PE (Test_Name : String) is
   begin
      Failed (Test_Name & " - Program_Error was INCORRECTLY raised");
   end Incorrect_PE;

   --  All the tests are designed to evaluate to 'equal'

   procedure Test_EQ (Test_Name : String; Flag : Boolean) is
   begin
      if Flag then
         --Comment (Test_Name & " -     equal");
         null;
      else
         Failed (Test_Name & " - not equal");
      end if;
   end Test_EQ;

   procedure Unexpected (Test_Name : String;
                         Err : Ada.Exceptions.Exception_Occurrence) is
   begin
      Failed (Test_Name & " - Unexpected run-time error raised - " &
              Ada.Exceptions.Exception_Name(Err));
      --Comment (Ada.Exceptions.Exception_Information(Err)); -- Debug.
   end Unexpected;

   --  27 tests presenting different combinations of Unchecked_Union,
   --  non-Unchecked_Union components and per-object constraints.

   function T001 return Boolean is
      type T2 is record                     --  Undiscriminanted
         Comp_T2_0 : Int := 0;              --  Regular
      end record;

      Obj1 : T2;                            --  Unconstrained
      Obj2 : T2;                            --  Unconstrained

   begin
      return Obj1 = Obj2;
   end T001;

   function T002 return Boolean is
      type T1 (I : Int := 0) is record
         case I is
            when 0 =>
               Comp_T1_1 : Int := 1;
            when others =>
               Comp_T1_2 : Int := 2;
         end case;
      end record;

      type T2 (D : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         Comp_T2_1 : T1;                    --  Explicit
      end record;

      Obj1 : T2;                            --  Unconstrained
      Obj2 : T2;                            --  Unconstrained

   begin
      return Obj1 = Obj2;
   end T002;

   function T003 return Boolean is
      type T1 (I : Int := 0) is record
         case I is
            when 0 =>
               Comp_T1_1 : Int := 1;
            when others =>
               Comp_T1_2 : Int := 2;
         end case;
      end record;

      type T2 (D : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         Comp_T2_1 : T1;                    --  Default
      end record;

      Obj1 : T2 (4);                        --  Constrained
      Obj2 : T2;                            --  Unconstrained

   begin
      return Obj1 /= Obj2;
   end T003;

   function T004 return Boolean is
      type T1 (I : Int := 0) is record
         case I is
            when 0 =>
               Comp_T1_1 : Int := 1;
            when others =>
               Comp_T1_2 : Int := 2;
         end case;
      end record;

      type T2 (D : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         Comp_T2_1 : T1 (1);                --  Explicit
      end record;

      Obj1 : T2;                            --  Unconstrained
      Obj2 : T2;                            --  Unconstrained

   begin
      return Obj1 = Obj2;
   end T004;

   function T005 return Boolean is
      type T1 (I : Int := 0) is record
         case I is
            when 0 =>
               Comp_T1_1 : Int := 1;
            when others =>
               Comp_T1_2 : Int := 2;
         end case;
      end record;

      type T2 (D : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         Comp_T2_1 : T1 (1);                --  Explicit
      end record;

      Obj1 : T2 (4);                        --  Constrained
      Obj2 : T2;                            --  Unconstrained

   begin
      return Obj1 /= Obj2;
   end T005;

   function T006 return Boolean is
      type T1 (I : Int := 0) is record
         case I is
            when 0 =>
               Comp_T1_1 : Int := 1;
            when others =>
               Comp_T1_2 : Int := 2;
         end case;
      end record;

      type T2 (D : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         Comp_T2_1 : T1 (D);                --  POC
      end record;

      Obj1 : T2;                            --  Unconstrained
      Obj2 : T2;                            --  Unconstrained

   begin
      return Obj1 = Obj2;
   end T006;

   function T007 return Boolean is
      type T1 (I : Int := 0) is record
         case I is
            when 0 =>
               Comp_T1_1 : Int := 1;
            when others =>
               Comp_T1_2 : Int := 2;
         end case;
      end record;

      type T2 (D : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         Comp_T2_1 : T1 (D);                --  POC
      end record;

      Obj1 : T2 (4);                        --  Constrained
      Obj2 : T2;                            --  Unconstrained

   begin
      return Obj1 /= Obj2;
   end T007;

   --  Must raise Program_Error since Comp_T2_1 lacks inferable discriminants
   function T008 return Boolean is
      type T1 (I : Int := 0) is record
         case I is
            when 0 =>
               Comp_T1_1 : Int := 1;
            when others =>
               Comp_T1_2 : Int := 2;
         end case;
      end record with Unchecked_Union;      -- N/A => ERROR.

      type T2 (D : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         Comp_T2_1 : T1;                    --  Default
      end record;

      Obj1 : T2;                            --  Unconstrained
      Obj2 : T2;                            --  Unconstrained

   begin
      return Obj1 = Obj2;
   end T008;

   --  Must raise Program_Error since Comp_T2_1 lacks inferable discriminants
   function T009 return Boolean is
      type T1 (I : Int := 0) is record
         case I is
            when 0 =>
               Comp_T1_1 : Int := 1;
            when others =>
               Comp_T1_2 : Int := 2;
         end case;
      end record with Unchecked_Union;

      type T2 (D : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         Comp_T2_1 : T1;                    --  Default
      end record;

      Obj1 : T2 (4);                        --  Constrained
      Obj2 : T2 (5);                        --  Constrained

   begin
      return Obj1 = Obj2;
   end T009;

   function T010 return Boolean is
      type T1 (I : Int := 0) is record
         case I is
            when 0 =>
               Comp_T1_1 : Int := 1;
            when others =>
               Comp_T1_2 : Int := 2;
         end case;
      end record with Unchecked_Union;

      type T2 (D : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         Comp_T2_1 : T1 (1);                --  Explicit
      end record;

      Obj1 : T2;                            --  Unconstrained
      Obj2 : T2;                            --  Unconstrained
      Obj3 : T2;                            --  Unconstrained
      Obj4 : T2;                            --  Unconstrained

   begin
      Obj3.Comp_T2_0 := 1;
      Obj4.Comp_T2_1.Comp_T1_2 := 1;

      return Obj1 = Obj2
               and then
             Obj1 /= Obj3
               and then
             Obj1 /= Obj4;
   end T010;

   function T011 return Boolean is
      type T1 (I : Int := 0) is record
         case I is
            when 0 =>
               Comp_T1_1 : Int := 1;
            when others =>
               Comp_T1_2 : Int := 2;
         end case;
      end record with Unchecked_Union;

      type T2 (D : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         Comp_T2_1 : T1 (1);                --  Explicit
      end record;

      Obj1 : T2 (4);                        --  Constrained
      Obj2 : T2 (5);                        --  Constrained
      Obj3 : T2 (Four);                     --  Constrained
      Obj4 : T2 (4);                        --  Constrained
      Obj5 : T2 (4);                        --  Constrained

   begin
      Obj4.Comp_T2_0 := 1;
      Obj5.Comp_T2_1.Comp_T1_2 := 1;

      return Obj1 /= Obj2
               and then
             Obj1 = Obj3
               and then
             Obj1 /= Obj4
               and then
             Obj1 /= Obj5;
   end T011;

   function T012 return Boolean is
      type T1 (I : Int := 0) is record
         case I is
            when 0 =>
               Comp_T1_1 : Int := 1;
            when others =>
               Comp_T1_2 : Int := 2;
         end case;
      end record with Unchecked_Union;

      type T2 (D : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         Comp_T2_1 : T1 (D);                --  POC
      end record;

      Obj1 : T2;                            --  Unconstrained
      Obj2 : T2;                            --  Unconstrained
      Obj3 : T2;                            --  Unconstrained
      Obj4 : T2;                            --  Unconstrained

   begin
      Obj3.Comp_T2_0 := 1;
      Obj4.Comp_T2_1.Comp_T1_1 := 2;

      return Obj1 = Obj2
               and then
             Obj1 /= Obj3
               and then
             Obj1 /= Obj4;
   end T012;

   function T013 return Boolean is
      type T1 (I : Int := 0) is record
         case I is
            when 0 =>
               Comp_T1_1 : Int := 1;
            when others =>
               Comp_T1_2 : Int := 2;
         end case;
      end record with Unchecked_Union;

      type T2 (D : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         Comp_T2_1 : T1 (D);                --  POC
      end record;

      Obj1 : T2 (4);                        --  Constrained
      Obj2 : T2 (5);                        --  Constrained
      Obj3 : T2 (Four);                     --  Constrained
      Obj4 : T2 (4);                        --  Constrained
      Obj5 : T2 (4);                        --  Constrained

   begin
      Obj4.Comp_T2_0 := 1;
      Obj5.Comp_T2_1.Comp_T1_2 := 1;

      return Obj1 /= Obj2
               and then
             Obj1 = Obj3
               and then
             Obj1 /= Obj4
               and then
             Obj1 /= Obj5;
   end T013;

   function T014 return Boolean is
      type T1 (I : Int := 0) is record
         case I is
            when 0 =>
               Comp_T1_1 : Int := 1;
            when others =>
               Comp_T1_2 : Int := 2;
         end case;
      end record;

      type T2 (D : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         case D is                          --  Variant part
            when 0 =>
               Comp_T2_2 : Int := 2;
            when others =>
               Comp_T2_3 : Int := 3;        --  Regular
         end case;
      end record;

      Obj1 : T2;                            --  Unconstrained
      Obj2 : T2;                            --  Unconstrained

   begin
      return Obj1 = Obj2;
   end T014;

   function T015 return Boolean is
      type T1 (I : Int := 0) is record
         case I is
            when 0 =>
               Comp_T1_1 : Int := 1;
            when others =>
               Comp_T1_2 : Int := 2;
         end case;
      end record;

      type T2 (D : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         case D is                          --  Variant part
            when 0 =>
               Comp_T2_2 : Int := 2;
            when others =>
               Comp_T2_3 : Int := 3;        --  Regular
         end case;
      end record;

      Obj1 : T2 (4);                        --  Constrained
      Obj2 : T2 (5);                        --  Constrained

   begin
      return Obj1 /= Obj2;
   end T015;

   function T016 return Boolean is
      type T1 (I : Int := 0) is record
         case I is
            when 0 =>
               Comp_T1_1 : Int := 1;
            when others =>
               Comp_T1_2 : Int := 2;
         end case;
      end record;

      type T2 (D : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         case D is                          --  Variant part
            when 0 =>
               Comp_T2_2 : Int := 2;
            when others =>
               Comp_T2_3 : T1;              --  Default
         end case;
      end record;

      Obj1 : T2;                            --  Unconstrained
      Obj2 : T2;                            --  Unconstrained

   begin
      return Obj1 = Obj2;
   end T016;

   function T017 return Boolean is
      type T1 (I : Int := 0) is record
         case I is
            when 0 =>
               Comp_T1_1 : Int := 1;
            when others =>
               Comp_T1_2 : Int := 2;
         end case;
      end record;

      type T2 (D : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         case D is                          --  Variant part
            when 0 =>
               Comp_T2_2 : Int := 2;
            when others =>
               Comp_T2_3 : T1;              --  Default
         end case;
      end record;

      Obj1 : T2 (4);                        --  Constrained
      Obj2 : T2;                            --  Unconstrained

   begin
      return Obj1 /= Obj2;
   end T017;

   function T018 return Boolean is
      type T1 (I : Int := 0) is record
         case I is
            when 0 =>
               Comp_T1_1 : Int := 1;
            when others =>
               Comp_T1_2 : Int := 2;
         end case;
      end record;

      type T2 (D : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         case D is                          --  Variant part
            when 0 =>
               Comp_T2_2 : Int := 2;
            when others =>
               Comp_T2_3 : T1 (3);          --  Explicit
         end case;
      end record;

      Obj1 : T2;                            --  Unconstrained
      Obj2 : T2;                            --  Unconstrained

   begin
      return Obj1 = Obj2;
   end T018;

   function T019 return Boolean is
      type T1 (I : Int := 0) is record
         case I is
            when 0 =>
               Comp_T1_1 : Int := 1;
            when others =>
               Comp_T1_2 : Int := 2;
         end case;
      end record;

      type T2 (D : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         case D is                          --  Variant part
            when 0 =>
               Comp_T2_2 : Int := 2;
            when others =>
               Comp_T2_3 : T1 (3);          --  Explicit
         end case;
      end record;

      Obj1 : T2 (4);                        --  Constrained
      Obj2 : T2;                            --  Unconstrained

   begin
      return Obj1 /= Obj2;
   end T019;

   function T020 return Boolean is
      type T1 (I : Int := 0) is record
         case I is
            when 0 =>
               Comp_T1_1 : Int := 1;
            when others =>
               Comp_T1_2 : Int := 2;
         end case;
      end record;

      type T2 (D : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         case D is                          --  Variant part
            when 0 =>
               Comp_T2_2 : Int := 2;
            when others =>
               Comp_T2_3 : T1 (D);          --  POC
         end case;
      end record;

      Obj1 : T2;                            --  Unconstrained
      Obj2 : T2;                            --  Unconstrained

   begin
      return Obj1 = Obj2;
   end T020;

   function T021 return Boolean is
      type T1 (I : Int := 0) is record
         case I is
            when 0 =>
               Comp_T1_1 : Int := 1;
            when others =>
               Comp_T1_2 : Int := 2;
         end case;
      end record;

      type T2 (D : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         case D is                          --  Variant part
            when 0 =>
               Comp_T2_2 : Int := 2;
            when others =>
               Comp_T2_3 : T1 (D);          --  POC
         end case;
      end record;

      Obj1 : T2 (4);                        --  Constrained
      Obj2 : T2;                            --  Unconstrained

   begin
      return Obj1 /= Obj2;
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
      end record with Unchecked_Union;

      type T2 (D : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         case D is                          --  Variant part
            when 0 =>
               Comp_T2_2 : Int := 2;
            when others =>
               Comp_T2_3 : T1;              --  Default
         end case;
      end record;

      Obj1 : T2;                            --  Unconstrained
      Obj2 : T2;                            --  Unconstrained
      Obj3 : T2;                            --  Unconstrained
      Obj4 : T2;                            --  Unconstrained

   begin
      Obj3.Comp_T2_0 := 1;
      Obj4 := (D => 4, Comp_T2_0 => 0, Comp_T2_3 => (I => 0, Comp_T1_1 => 2));

      return Obj1 = Obj2
               and then
             Obj1 /= Obj3
               and then
             Obj1 /= Obj4;
   end T022;

   --  Must raise Program_Error since Comp_T2_3 lacks inferable discriminants
   function T023 return Boolean is
      type T1 (I : Int := 0) is record
         case I is
            when 0 =>
               Comp_T1_1 : Int := 1;
            when others =>
               Comp_T1_2 : Int := 2;
         end case;
      end record with Unchecked_Union;

      type T2 (D : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         case D is                          --  Variant part
            when 0 =>
               Comp_T2_2 : Int := 2;
            when others =>
               Comp_T2_3 : T1;              --  Default
         end case;
      end record;

      Obj1 : T2 (4);                        --  Constrained
      Obj2 : T2 (5);                        --  Constrained
      Obj3 : T2 (Four);                     --  Constrained
      Obj4 : T2 (4);                        --  Constrained
      Obj5 : T2 (4);                        --  Constrained

   begin
      Obj4.Comp_T2_0 := 1;
      Obj5.Comp_T2_3.Comp_T1_1 := 2;

      return Obj1 /= Obj2
               and then
             Obj1 = Obj3
               and then
             Obj1 /= Obj4
               and then
             Obj1 /= Obj5;
   end T023;

   function T024 return Boolean is
      type T1 (I : Int := 0) is record
         case I is
            when 0 =>
               Comp_T1_1 : Int := 1;
            when others =>
               Comp_T1_2 : Int := 2;
         end case;
      end record with Unchecked_Union;

      type T2 (D : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         case D is                          --  Variant part
            when 0 =>
               Comp_T2_2 : Int := 2;
            when others =>
               Comp_T2_3 : T1 (3);          --  Explicit
         end case;
      end record;

      Obj1 : T2;                            --  Unconstrained
      Obj2 : T2;                            --  Unconstrained
      Obj3 : T2;                            --  Unconstrained
      Obj4 : T2;                            --  Unconstrained

   begin
      Obj3.Comp_T2_0 := 1;
      Obj4 := (D => 1, Comp_T2_0 => 0, Comp_T2_3 => (I => 3, Comp_T1_2 => 1));

      return Obj1 = Obj2
               and then
             Obj1 /= Obj3
               and then
             Obj1 /= Obj4;
   end T024;

   function T025 return Boolean is
      type T1 (I : Int := 0) is record
         case I is
            when 0 =>
               Comp_T1_1 : Int := 1;
            when others =>
               Comp_T1_2 : Int := 2;
         end case;
      end record with Unchecked_Union;

      type T2 (D : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         case D is                          --  Variant part
            when 0 =>
               Comp_T2_2 : Int := 2;
            when others =>
               Comp_T2_3 : T1 (3);          --  Explicit
         end case;
      end record;

      Obj1 : T2 (4);                        --  Constrained
      Obj2 : T2 (5);                        --  Constrained
      Obj3 : T2 (Four);                     --  Constrained
      Obj4 : T2 (4);                        --  Constrained
      Obj5 : T2 (4);                        --  Constrained

   begin
      Obj4.Comp_T2_0 := 1;
      Obj5.Comp_T2_3.Comp_T1_2 := 1;

      return Obj1 /= Obj2
               and then
             Obj1 = Obj3
               and then
             Obj1 /= Obj4
               and then
             Obj1 /= Obj5;
   end T025;

   function T026 return Boolean is
      type T1 (I : Int := 0) is record
         case I is
            when 0 =>
               Comp_T1_1 : Int := 1;
            when others =>
               Comp_T1_2 : Int := 2;
         end case;
      end record with Unchecked_Union;

      type T2 (D : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         case D is                          --  Variant part
            when 0 =>
               Comp_T2_2 : Int := 2;
            when others =>
               Comp_T2_3 : T1 (D);          --  POC
         end case;
      end record;

      Obj1 : T2;                            --  Unconstrained
      Obj2 : T2;                            --  Unconstrained
      Obj3 : T2;                            --  Unconstrained
      Obj4 : T2;                            --  Unconstrained

   begin
      Obj3.Comp_T2_0 := 1;
      Obj4 := (D => 1, Comp_T2_0 => 0, Comp_T2_3 => (I => 1, Comp_T1_2 => 2));

      return Obj1 = Obj2
               and then
             Obj1 /= Obj3
               and then
             Obj1 /= Obj4;
   end T026;

   function T027 return Boolean is
      type T1 (I : Int := 0) is record
         case I is
            when 0 =>
               Comp_T1_1 : Int := 1;
            when others =>
               Comp_T1_2 : Int := 2;
         end case;
      end record with Unchecked_Union;

      type T2 (D : Int := 0) is record      --  Discriminated
         Comp_T2_0 : Int := 0;              --  Regular
         case D is                          --  Variant part
            when 0 =>
               Comp_T2_2 : Int := 2;
            when others =>
               Comp_T2_3 : T1 (D);          --  POC
         end case;
      end record;

      Obj1 : T2 (4);                        --  Constrained
      Obj2 : T2 (5);                        --  Constrained
      Obj3 : T2 (Four);                     --  Constrained
      Obj4 : T2 (4);                        --  Constrained
      Obj5 : T2 (4);                        --  Constrained

   begin
      Obj4.Comp_T2_0 := 1;
      Obj5.Comp_T2_3.Comp_T1_2 := 1;

      return Obj1 /= Obj2
               and then
             Obj1 = Obj3
               and then
             Obj1 /= Obj4
               and then
             Obj1 /= Obj5;
   end T027;

begin
   Test ("CXB3020", "Check that Program_Error is raised by the predefined " &
                    "equality operator for any type that has a subcomponent " &
                    "of an unchecked union type whose nominal subtype is " &
                    "unconstrained");

   begin
      Test_EQ ("T001", T001);
   exception
      when Program_Error =>
         Incorrect_PE ("T001");
      when Err:others =>
         Unexpected ("T001", Err);
   end;

   begin
      Test_EQ ("T002", T002);
   exception
      when Program_Error =>
         Incorrect_PE ("T002");
      when Err:others =>
         Unexpected ("T002", Err);
   end;

   begin
      Test_EQ ("T003", T003);
   exception
      when Program_Error =>
         Incorrect_PE ("T003");
      when Err:others =>
         Unexpected ("T003", Err);
   end;

   begin
      Test_EQ ("T004", T004);
   exception
      when Program_Error =>
         Incorrect_PE ("T004");
      when Err:others =>
         Unexpected ("T004", Err);
   end;

   begin
      Test_EQ ("T005", T005);
    exception
      when Program_Error =>
         Incorrect_PE ("T005");
      when Err:others =>
         Unexpected ("T005", Err);
   end;

   begin
      Test_EQ ("T006", T006);
   exception
      when Program_Error =>
         Incorrect_PE ("T006");
      when Err:others =>
         Unexpected ("T006", Err);
   end;

   begin
      Test_EQ ("T007", T007);
   exception
      when Program_Error =>
         Incorrect_PE ("T007");
      when Err:others =>
         Unexpected ("T007", Err);
   end;

   begin
      Test_EQ ("T008", T008);
      Failed ("T008 Exception not raised");
   exception
      when Program_Error =>
         Correct_PE ("T008");
      when Err:others =>
         Unexpected ("T008", Err);
   end;

   begin
      Test_EQ ("T009", T009);
      Failed ("T009 Exception not raised");
   exception
      when Program_Error =>
         Correct_PE ("T009");
      when Err:others =>
         Unexpected ("T009", Err);
   end;

   begin
      Test_EQ ("T010", T010);
   exception
      when Program_Error =>
         Incorrect_PE ("T010");
      when Err:others =>
         Unexpected ("T010", Err);
   end;

   begin
      Test_EQ ("T011", T011);
   exception
      when Program_Error =>
         Incorrect_PE ("T011");
      when Err:others =>
         Unexpected ("T011", Err);
   end;

   begin
      Test_EQ ("T012", T012);
   exception
      when Program_Error =>
         Incorrect_PE ("T012");
      when Err:others =>
         Unexpected ("T012", Err);
   end;

   begin
      Test_EQ ("T013", T013);
   exception
      when Program_Error =>
         Incorrect_PE ("T013");
      when Err:others =>
         Unexpected ("T013", Err);
   end;

   begin
      Test_EQ ("T014", T014);
   exception
      when Program_Error =>
         Incorrect_PE ("T014");
      when Err:others =>
         Unexpected ("T014", Err);
   end;

   begin
      Test_EQ ("T015", T015);
   exception
      when Program_Error =>
         Incorrect_PE ("T015");
      when Err:others =>
         Unexpected ("T015", Err);
   end;

   begin
      Test_EQ ("T016", T016);
   exception
      when Program_Error =>
         Incorrect_PE ("T016");
      when Err:others =>
         Unexpected ("T016", Err);
   end;

   begin
      Test_EQ ("T017", T017);
   exception
      when Program_Error =>
         Incorrect_PE ("T017");
      when Err:others =>
         Unexpected ("T017", Err);
   end;

   begin
      Test_EQ ("T018", T018);
   exception
      when Program_Error =>
         Incorrect_PE ("T018");
      when Err:others =>
         Unexpected ("T018", Err);
   end;

   begin
      Test_EQ ("T021", T021);
   exception
      when Program_Error =>
         Incorrect_PE ("T021");
      when Err:others =>
         Unexpected ("T021", Err);
   end;

   begin
      Test_EQ ("T022", T022);
      Failed ("T022 Exception not raised");
   exception
      when Program_Error =>
         Correct_PE ("T022");
      when Err:others =>
         Unexpected ("T022", Err);
   end;

   begin
      Test_EQ ("T023", T023);
      Failed ("T023 Exception not raised");
   exception
      when Program_Error =>
         Correct_PE ("T023");
      when Err:others =>
         Unexpected ("T023", Err);
   end;

   begin
      Test_EQ ("T024", T024);
   exception
      when Program_Error =>
         Incorrect_PE ("T024");
      when Err:others =>
         Unexpected ("T024", Err);
   end;

   begin
      Test_EQ ("T025", T025);
   exception
      when Program_Error =>
         Incorrect_PE ("T025");
      when Err:others =>
         Unexpected ("T025", Err);
   end;

   begin
      Test_EQ ("T026", T026);
   exception
      when Program_Error =>
         Incorrect_PE ("T026");
      when Err:others =>
         Unexpected ("T026", Err);
   end;

   begin
      Test_EQ ("T027", T027);
   exception
      when Program_Error =>
         Incorrect_PE ("T027");
      when Err:others =>
         Unexpected ("T027", Err);
   end;

   Result;
end CXB3020;
