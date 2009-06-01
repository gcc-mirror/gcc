package OCONST5 is

  type u1 is mod 2**1;
  type u8 is mod 2**8;

  type HI_Record is record
    A, B : U8;
  end record;
  pragma Suppress_Initialization (HI_Record);

  type R is record
     Bit : U1;
     Agg : HI_Record;
  end record;
  pragma Suppress_Initialization (R);

  for R use record
     Bit at 0 range  0 .. 0;
     Agg at 0 range  1 .. 16;
  end record;

  My_R0 : R := (Bit => 0, Agg => (A => 3, B => 7));
  My_R1 : R := (Bit => 1, Agg => (A => 3, B => 7));

  procedure Check (Arg : R; Bit : U1);

end;
