-- { dg-do compile }
-- { dg-options "-O -gnatVT -fdump-tree-optimized" }

--  Check that we perform the expected validity checks for
--  hardbool-annotated types, even when checking of tests is disabled.

package body Hardbool is
   function T return Boolean is (Boolean (X) and then Boolean (Y));

   procedure P1 is
   begin
      X := HBool1 (not Y);
   end P1;

   procedure P2 is
   begin
      X := HBool1 (if Y then HBool2'(False) else HBool2'(True));
   end P2;

   procedure P3 is
   begin
      X := (if Y then HBool1'(False) else HBool1'(True));
   end P3;

   procedure Q1 is
   begin
      Y := HBool2 (not X);
   end Q1;

   procedure Q2 is
   begin
      Y := HBool2 (if X then HBool1'(False) else HBool1'(True));
   end Q2;

   procedure Q3 is
   begin
      Y := (if X then HBool2'(False) else HBool2'(True));
   end Q3;

end Hardbool;

-- One for each type's _rep_to_pos function.
-- { dg-final { scan-tree-dump-times "gnat_rcheck_CE_Invalid_Data ..hardbool.ads" 2 "optimized" } }

-- One check for each variable used in T, one use in each P* and in each Q*.
-- { dg-final { scan-tree-dump-times "gnat_rcheck_CE_Invalid_Data ..hardbool.adb" 8 "optimized" } }
