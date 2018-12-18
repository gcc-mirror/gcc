with Atomic11_Pkg2;

package Atomic11_Pkg1 is

  type Rec1 is record
    I : Integer;
  end record;

  procedure Proc1 (R : Rec1);
  pragma Import (C, Proc1);

  type Arr is array (Positive range <>) of Integer;

  type Rec2 is record
    A : Arr (1 .. Atomic11_Pkg2.Max);
  end record;

  procedure Proc2 (R : Rec2);

end Atomic11_Pkg1;
