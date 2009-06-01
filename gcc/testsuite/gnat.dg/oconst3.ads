package OCONST3 is

  type bit is (zero, one);
  type u8 is mod 2**8;

  type Base is record
    i1 : Integer;
  end Record;

  type R is record
    u : u8;
    f : bit;
    b : Base;
  end record;

  for R use record
    u at 0 range 0 .. 7;
    f at 1 range 0 .. 0;
    b at 1 range 1 .. 32;  -- unaligned SImode bitfield
  end record;

  My_R : constant R := (u=>1, f=>one, b=>(i1=>3));

  procedure check (arg : R);

end;
