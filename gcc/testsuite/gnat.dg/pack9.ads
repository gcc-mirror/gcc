package Pack9 is

  type R1 is record
    I : Integer;
    C : Character;
  end record;

  type R2 is record
    I1, I2 : Integer;
    A : R1;
  end record;
  pragma Pack(R2);

  type R2_Ptr is access all R2;

  procedure Copy (X, Y : R2_Ptr);

end Pack9;
