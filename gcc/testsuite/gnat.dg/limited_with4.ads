limited with Limited_With4_Pkg;

package Limited_With4 is

  type Ptr1 is access procedure (A : Limited_With4_Pkg.Rec12; I : Integer);

  type Ptr2 is access procedure (A : Limited_With4_Pkg.Rec22; I : Integer);

  type Rec1 is record
    I : Integer;
  end record;

  procedure Proc1 (A : Limited_With4_Pkg.Rec12 ; I : Integer);

  function Func1 (I : Integer) return Limited_With4_Pkg.Rec12;

  procedure Proc2 (A : Limited_With4_Pkg.Rec22 ; I : Integer);

  function Func2 (I : Integer) return Limited_With4_Pkg.Rec22;

  type Rec2 is record
    I : Integer;
  end record;

  procedure Proc3 (A : Limited_With4_Pkg.Rec12 ; B : Limited_With4_Pkg.Rec22);

  function Func3 (A : Limited_With4_Pkg.Rec12) return Limited_With4_Pkg.Rec22;

end Limited_With4;
