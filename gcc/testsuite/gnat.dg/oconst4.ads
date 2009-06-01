package OCONST4 is

  type bit is (zero, one);
  type u2 is mod 2**2;
  type u5 is mod 2**5;
  type u8 is mod 2**8;

  type Base is record
    f1 : bit;
    f2 : u2;
    f3 : u5;
    f4 : u8;
  end record;

  for Base use record
    f1 at 0 range  0 .. 0;
    f2 at 0 range  1 .. 2;
    f3 at 0 range  3 .. 7;
    f4 at 1 range  0 .. 7;
  end record;

  type Derived is record
    f1 : u5;
    b  : Base;
    f2 : bit;
    f3 : u2;
    f4 : u8;
    i1 : Integer;
    i2 : Integer;
  end record;

  for Derived use record
    f1 at 0 range  0 ..  4;
    b  at 0 range  5 .. 20;  -- unaligned HImode bitfield
    f2 at 0 range 21 .. 21;
    f3 at 0 range 22 .. 23;
    f4 at 0 range 24 .. 31;
    i1 at 4 range  0 .. 31;
    i2 at 8 range  0 .. 31;
  end record;

  type R is record
    u : u8;
    d : Derived;
  end record;

  for R use record
    u at 0 range 0 .. 7;
    d at 1 range 0 .. 95;  -- BLKmode bitfield
  end record;

  My_R : constant R := (u=>1,
                        d=>(f1=>17,
                            b=>(f1=>one,
                                f2=>2,
                                f3=>17,
                                f4=>42),
                            f2=>one,
                            f3=>1,
                            f4=>111,
                            i1=>2,
                            i2=>3));

  procedure check (arg : R);

end;
