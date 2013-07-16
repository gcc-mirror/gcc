-- { dg-do compile }

package Aggr6 is

  type B15_T is mod 2 ** 15;
  for B15_T'Size use 15;
  for B15_T'Alignment use 1;

  type B17_T is mod 2 ** 17;
  for B17_T'Size use 17;
  for B17_T'Alignment use 1;

  type Rec_T is record
    A : B17_T;
    B : B15_T;
  end record;
  for Rec_T use record
    A at 0 range 0 .. 16;
    B at 0 range 17 .. 31;
  end record;
  for Rec_T'Size use 32;

  C : constant Rec_T := (A => 1, B => 0);

end Aggr6;
