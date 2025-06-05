-- { dg-do compile }
-- { dg-options "-O2 -gnatn" }

with Opt7_Pkg; use Opt7_Pkg;

package Opt7 is

  type Rec is record
    E : Enum;
  end record;

  function Image (R : Rec) return String is
    (if R.E = A then Image (R.E) else "");

end Opt7;
