-- { dg-do compile }

package Size_Clause1 is

  -- The record inherits the alignment of Integer, which is 4, so
  -- the real size is 64 instead of 40.  That's OK, as long as the
  -- size of a component of type R1 in a packed record is 40.
  type R1 is record
    I : Integer;
    B : Boolean;
  end record;
  for R1'Size use 40;

  type S1 is record
    rr : R1; -- size must be 40
  end record;
  pragma Pack(S1);

  -- The record is explicitly given alignment 1 so its real type is 40 too.
  -- The size of a component of type R2 in a packed record is naturally 40.
  type R2 is record
    I : Integer;
    B : Boolean;
  end record;
  for R2'Size use 40;
  for R2'Alignment use 1;

  type S2 is record
    rr : R2; -- size must be 40
  end record;
  pragma Pack(S2);

  -- The record is explicitly given alignment 4 so its real type is 64.
  -- That's OK, as long as the size of a component of type R3 in a packed
  -- record is 40.
  type R3 is record
    I : Integer;
    B : Boolean;
  end record;
  for R3'Size use 40;
  for R3'Alignment use 4;

  type S3 is record
    rr : R3; -- size must be 40
  end record;
  pragma Pack(S3);

end Size_Clause1;
