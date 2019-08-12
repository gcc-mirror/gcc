package Equal10 is
  type R is record X : Integer; end record;
  Rr : R;
  function "=" (Y : R; Z : Integer) return Boolean is
     (Y.X = Z);
  procedure Dummy;
end Equal10;
