-- { dg-do run { target hppa*-*-* sparc*-*-* powerpc-*-* powerpc64-*-* } }

with Unchecked_Conversion;

procedure Unchecked_Convert5 is

  subtype c_1 is string(1..1);

  function int2c1 is  -- { dg-warning "different sizes" }
    new unchecked_conversion (source => integer, target => c_1);

  c1 : c_1;

begin

  c1 := int2c1(16#12#);

  if c1 (1) /= ASCII.Nul then
    raise Program_Error;
  end if;

end;
