-- { dg-do compile }
-- { dg-options "-O2" }

package Pack12 is

  type Rec1 is record
    B : Boolean;
    N : Natural;
  end record;

  type Rec2 is record
    B : Boolean;
    R : Rec1;
  end record;
  pragma Pack (Rec2);

  type Rec3 is tagged record
    R : Rec2;
  end record;

end Pack12;
