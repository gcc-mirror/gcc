-- { dg-do compile }

package Discr7 is

  type Enum is (One, Two, Three);
  for Enum use (One => 1, Two => 2, Three => 3);

  type Arr is array (Integer range <>, Enum range <>) of Boolean;

  type Rec (D : Integer) is record
    A: Arr (1 .. D, Enum'Range);
  end record;

end Discr7;
