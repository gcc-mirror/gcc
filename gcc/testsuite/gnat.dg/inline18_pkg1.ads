with Inline18_Pkg2.Child;
with Inline18_Gen2;
with Inline18_Gen3;

package Inline18_Pkg1 is

  package My_G2 is new Inline18_Gen2 (Inline18_Pkg2.Child.General.T);

  package My_G3 is new Inline18_Gen3 (Integer);

  type Rec is record
    Comp : My_G2.T;
  end record;

  procedure Proc (R : in out Rec);

  package My_G is new My_G3.Inner_G;

end Inline18_Pkg1;
