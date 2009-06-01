package OCONST2 is

  type u8 is mod 2**8;

  type Base is record
    i1 : Integer;
  end Record;

  type R is record
    u : u8;
    b : Base;
  end record;

  for R use record
    u at 0 range 0 .. 7;
    b at 1 range 0 .. 31;  -- aligned SImode bitfield
  end record;

  My_R : constant R := (u=>1, b=>(i1=>2));

  procedure check (arg : R);

end;
