-- { dg-do compile }
-- { dg-options "-gnatws" }

pragma Restrictions (No_Elaboration_Code);

package Array2 is

  type Arr is array (Positive range <>) of Boolean;
  A : Arr (1 .. 2 ** 2);
  for A'Size use 16#1000_0000_0#;

end Array2;
