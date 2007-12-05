-- { dg-do compile }

pragma Restrictions(No_Elaboration_Code);

with System;

package Elab1 is

   type Ptrs_Type is array (Integer range 1 .. 2) of System.Address;
   type Vars_Array is array (Integer range 1 .. 2) of Integer;

   Vars : Vars_Array;

   Val1 : constant Integer := 1;
   Val2 : constant Integer := 2;

   Ptrs : constant Ptrs_Type :=
     (1  => Vars (Val1)'Address,
      2  => Vars (Val2)'Address);

end Elab1;
