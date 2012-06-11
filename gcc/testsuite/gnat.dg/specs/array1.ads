-- { dg-do compile }

pragma Restrictions (No_Elaboration_Code);

package Array1 is

  type Arr is array (Positive range <>) of Boolean;
  A : Arr (1 .. 2 ** 29);

end Array1;
