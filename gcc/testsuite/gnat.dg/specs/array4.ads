-- { dg-do compile }
-- { dg-skip-if "missing -gsplit-dwarf support" { *-*-darwin* } }
-- { dg-options "-gsplit-dwarf" }

package Array4 is

  type Arr1 is array (Positive range <>) of Boolean;

  Size : Positive := 20;

  type Rec is record
    A : Arr1 (1 .. Size);
  end record;

  type Arr2 is array (Positive range <>) of Rec;

end Array4;
