package Slice2 is

  type R1 is record
    Text : String (1 .. 30);
  end record;

  type R2 is record
    Text : String (1 .. 8);
    B : Boolean := True;
  end record;

  function F (I : R1) return R2;

end Slice2;
