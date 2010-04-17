-- { dg-do run }

procedure Sizetype2 is

  function Ident_Int (X : Integer) return Integer is
  begin
     return X;
  end;

  type A is array (Integer range <>) of Boolean;
  subtype T1 is A (Ident_Int (- 6) .. Ident_Int (Integer'Last - 4));
  subtype T2 is A (- 6 .. Ident_Int (Integer'Last - 4));
  subtype T3 is A (Ident_Int (- 6) .. Integer'Last - 4);

begin
  if T1'Size /= 17179869200 then
    raise Program_Error;
  end if;

  if T2'Size /= 17179869200 then
    raise Program_Error;
  end if;

  if T3'Size /= 17179869200 then
    raise Program_Error;
  end if;
end;
