-- { dg-do run { target i?86-*-* x86_64-*-* alpha*-*-* ia64-*-* } }

with Unchecked_Conversion;

procedure Unchecked_Convert5b is

  subtype c_1 is string(1..1);

  function int2c1 is  -- { dg-warning "different sizes" }
    new unchecked_conversion (source => integer, target => c_1);

  c1 : c_1;

begin

  c1 := int2c1(16#12#);

  if c1 (1) /= ASCII.DC2 then
    raise Program_Error;
  end if;

end;
