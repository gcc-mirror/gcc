-- { dg-do compile }

package Size_Clause3 is

  -- The record inherits the alignment of Integer, which is 4, so
  -- the real size is 64 instead of 40.
  type R1 is record
    I : Integer;
    B : aliased Boolean;
  end record;

  -- That's not OK, the size of a component of type R1 cannot be 40.
  type S1 is record
    rr : R1; -- size must be 40
  end record;
  for S1 use record
    rr at 0 range 0 .. 39;  -- { dg-error "size for .rr. with aliased part too small" }
  end record;

  -- The record is explicitly given alignment 1 so its real type is 40.
  type R2 is record
    I : Integer;
    B : aliased Boolean;
  end record;
  for R2'Alignment use 1;

  -- That's OK, the size of a component of type R2 can be 40.
  type S2 is record
    rr : R2; -- size must be 40
  end record;
  for S2 use record
    rr at 0 range 0 .. 39;
  end record;

  -- The record is explicitly given alignment 4 so its real type is 64.
  type R3 is record
    I : Integer;
    B : aliased Boolean;
  end record;
  for R3'Alignment use 4;

  -- That's not OK, the size of a component of type R3 cannot be 40.
  type S3 is record
    rr : R3; -- size must be 40
  end record;
  for S3 use record
    rr at 0 range 0 .. 39;  -- { dg-error "size for .rr. with aliased part too small" }
  end record;

end Size_Clause3;
