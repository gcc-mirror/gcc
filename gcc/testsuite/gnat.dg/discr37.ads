package Discr37 is

  subtype Index is Integer range 0 .. 100;

  type Root;
  type Frame_Ptr is access all Root'Class;

  type Arr is array (Index range <>) of Frame_Ptr;

  type Root (Level : Index) is tagged record
    S : Arr (0 .. Level);
  end record;

  type Derived (Level : Index) is new Root (Level) with null record;

  type Child is new Derived (0) with record
    F : Arr (0 .. 100);
  end record;

  procedure Proc (A : access Child);

end Discr37;
