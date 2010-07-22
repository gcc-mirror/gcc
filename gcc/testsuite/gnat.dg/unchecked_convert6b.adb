-- { dg-do run { target i?86-*-* x86_64-*-* alpha*-*-* ia64-*-* } }

with Unchecked_Conversion;

procedure Unchecked_Convert6b is

  subtype c_5 is string(1..5);

  function int2c5 is  -- { dg-warning "different sizes" }
    new unchecked_conversion (source => integer, target => c_5);

  c5 : c_5;

begin

  c5 := int2c5(16#12#);

  if c5 (1) /= ASCII.DC2 then
    raise Program_Error;
  end if;

end;
