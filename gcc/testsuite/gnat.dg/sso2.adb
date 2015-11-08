-- { dg-do compile }

with Ada.Unchecked_Conversion;

package body SSO2 is

  function Conv is new Ada.Unchecked_Conversion (Arr1, Arr2);

  procedure Proc (A1 : Arr1; A2 : out Arr2) is
  begin
     A2 := Conv (A1);
  end;

end SSO2;
