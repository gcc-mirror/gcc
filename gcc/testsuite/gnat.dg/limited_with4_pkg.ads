with Limited_With4;

package Limited_With4_Pkg is

  P1 : Limited_With4.Ptr1 := Limited_With4.Proc1'Access;

  P2 : Limited_With4.Ptr2 := Limited_With4.Proc2'Access;

  type Rec12 is record
    I : Integer;
    R : Limited_With4.Rec1;
  end record;

  type Rec22 is record
    I : Integer;
    R : Limited_With4.Rec2;
  end record;

end Limited_With4_Pkg;
