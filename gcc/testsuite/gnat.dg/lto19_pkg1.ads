with Lto19_Pkg2;

package Lto19_Pkg1 is

  type Arr is array (1 .. Lto19_Pkg2.UB) of Integer;

  type Rec is record
    A : Arr;
    I : Integer;
  end record;

  procedure Proc (R : Rec);

end Lto19_Pkg1;
