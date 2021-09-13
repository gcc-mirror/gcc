-- { dg-do compile }
-- { dg-options "-flto" { target lto } }

package Lto25 is

  type Enum is (One, Two, Three) with Atomic;

  type Rec is record
    E : Enum := One;
  end record;

end Lto25;
