-- C680001.A
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
--     Check that legal uses of an expression function as a completion
--     work as expected.
--
-- TEST DESCRIPTION:
--     This Ada 2012 test tests the use of expression functions as completions.
--
--     The test declares three copies of the same set of 4 functions.
--     The first set is declared and completed in a package visible part,
--     the second set is declared in a visible part and completed in the
--     private part, and the third set is declared in a visible part and
--     completed in the package body. The various copies should behave
--     identically. Each of the four functions (except the first) calls
--     the preceding one.
--
--     After checking the results, an additional test case tests that
--     elaboration checking is performed as usual for expression functions
--     that are completions.
--
-- CHANGE HISTORY:
--     27 Sep 13   SWB   Initial version.
--     17 Oct 13   RLB   Fixed header.
--     20 Mar 14   RLB   Readied to issue; minor changes to comments.
--!

with Report;

procedure C680001 is
   type Index is mod 11 with Default_Value => 0;
   Index_Var : Index;
   type Vector is array (Index) of aliased Index;
   Vector_Var : Vector;

   procedure Reset_State (Count : Positive) is
   begin
      Index_Var := 0;
      Vector_Var := (others => 0);
      for C in 1 .. Count loop
         for I in Index loop
            Vector_Var (I) := 3 * Vector_Var (I) + 1;
            Index_Var := Index_Var + Vector_Var (I);
         end loop;
      end loop;
   end Reset_State;

   package Completed_In_Spec is
      function F001 return Index;
      function F002 return access Index;
      generic
         with function F1 return Index;
         with function F2 return access Index;
      function F003_G return Index;
      generic
         type T is private;
         type T_Vector is array (Index) of aliased T;
         Vector_Var : in out T_Vector;
         with function ">"  (L : T; R : Index) return Boolean is <>;
      function F004_G (X : Index := 0) return access T;

      function F001 return Index is (Index_Var); -- simple case

      function F002 return access Index is (Vector_Var (F001)'Access);
        -- anonymous access-to-object result type case

      function F003_G return Index is (F1 + F2.all); -- generic case
      function F003 is new F003_G (F001, F002);

      function F004_G (X : Index := 0) return access T is
        (if Vector_Var (X) > X then F004_G (X + 1)
                               else Vector_Var (F003)'Access);
        -- generic, recursive, anonymous-to-formal-type result type case
      function F004 is new F004_G (Index, Vector, Vector_Var);
   end Completed_In_Spec;

   package Completed_In_Private_Part is
      function F001 return Index;
      function F002 return access Index;
      generic
         with function F1 return Index;
         with function F2 return access Index;
      function F003_G return Index;
      generic
         type T is private;
         type T_Vector is array (Index) of aliased T;
         Vector_Var : in out T_Vector;
         with function ">"  (L : T; R : Index) return Boolean is <>;
      function F004_G (X : Index := 0) return access T;
      function F004_Wrapper (X : Index := 0) return access Index;
   private
      function F001 return Index is (Index_Var); -- simple case

      function F002 return access Index is (Vector_Var (F001)'Access);
        -- anonymous access-to-object result type case

      function F003_G return Index is (F1 + F2.all); -- generic case
      function F003 is new F003_G (F001, F002);

      function F004_G (X : Index := 0) return access T is
        (if Vector_Var (X) > X then F004_G (X + 1)
                               else Vector_Var (F003)'Access);
        -- generic, recursive, anonymous-to-formal-type result type case
      function F004 is new F004_G (Index, Vector, Vector_Var);
      function F004_Wrapper (X : Index := 0) return access Index renames F004;
   end Completed_In_Private_Part;

   package Completed_In_Body is
      function F001 return Index;
      function F002 return access Index;
      generic
         with function F1 return Index;
         with function F2 return access Index;
      function F003_G return Index;
      generic
         type T is private;
         type T_Vector is array (Index) of aliased T;
         Vector_Var : in out T_Vector;
         with function ">"  (L : T; R : Index) return Boolean is <>;
      function F004_G (X : Index := 0) return access T;
      function F004_Wrapper (X : Index := 0) return access Index;
   end;

   package body Completed_In_Body is
      function F001 return Index is (Index_Var); -- simple case

      function F002 return access Index is (Vector_Var (F001)'Access);
        -- anonymous access-to-object result type case

      function F003_G return Index is (F1 + F2.all); -- generic case
      function F003 is new F003_G (F001, F002);

      function F004_G (X : Index := 0) return access T is
        (if Vector_Var (X) > X then F004_G (X + 1)
                               else Vector_Var (F003)'Access);
        -- generic, recursive, anonymous-to-formal-type result type case
      function F004 is new F004_G (Index, Vector, Vector_Var);
      function F004_Wrapper (X : Index := 0) return access Index renames F004;
   end Completed_In_Body;

begin
   Report.Test ("C680001", "Check expression functions as completions");

   declare
      Expected_Result : Index := 4;
      type Completion_Site is (Vis, Priv, Bod);
      Result : Index;
   begin
      for Site in Completion_Site loop
        Reset_State (7);
        Result :=
          (case Site is
             when Vis => Completed_In_Spec.F004.all,
             when Priv => Completed_In_Private_Part.F004_Wrapper.all,
             when Bod => Completed_In_Body.F004_Wrapper.all);
        if Result /= Expected_Result then
           Report.Failed
             ("Wrong result; Site = "
              & Completion_Site'Image (Site)
              & "; Result =" & Index'Image (Result)
              & "; Expected_Result = " & Index'Image (Expected_Result));
         end if;
      end loop;
   end;

   declare
      --  Test elaboration checking for a function which is
      --  completed as an expression function.

      function Zero return Integer;
      Zero_Result : Natural;
      type Func_Ref is access function return Integer;
      Zero_Ref : Func_Ref := Zero'Access;
      Early_Value : constant := 123;

      function Call_Zero
        (Too_Early : Boolean) return Integer is
      begin
         Zero_Result := Zero;
         if Too_Early then
            Report.Failed ("Elaboration check omitted");
         end if;
         return Zero_Result;
      exception
         when Program_Error =>
            if not Too_Early then
               raise;
            end if;
            return Early_Value;
      end Call_Zero;

      function Call_Zero_Indirectly
        (Too_Early : Boolean) return Integer is
      begin
         Zero_Result := Zero_Ref.all;
         if Too_Early then
            Report.Failed ("Elaboration check omitted - indirect case");
         end if;
         return Zero_Result;
      exception
         when Program_Error =>
            if not Too_Early then
               raise;
            end if;
            return Early_Value;
      end Call_Zero_Indirectly;

      X1 : constant Integer := Call_Zero (Too_Early => True);
      X2 : constant Integer := Call_Zero_Indirectly (Too_Early => True);

      function Zero return Integer is (0);

      X3 : constant Integer := Call_Zero (Too_Early => False);
      X4 : constant Integer := Call_Zero_Indirectly (Too_Early => False);
   begin
      if (X1 /= Early_Value) or (X2 /= Early_Value) then
         Report.Failed ("Access-before-elaboration not detected");
      end if;
      if (X3 /= 0) or (X4 /= 0) then
         Report.Failed ("Unexpected Call_Zero results");
      end if;
   end;

   Report.Result;
end C680001;
