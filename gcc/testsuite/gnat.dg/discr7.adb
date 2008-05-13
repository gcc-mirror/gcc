-- { dg-do compile }

procedure Discr7 is

  subtype Index is Natural range 0..5;
  type BitString is array(Index range <>) of Boolean;
  pragma Pack(BitString);

  function Id (I : Integer) return Integer is
  begin
    return I;
  end;

  type E(D : Index) is record
    C : BitString(1..D);
  end record;

  subtype E0 is E(Id(0));

  function F return E0 is
  begin
    return E'(D=>0, C=>(1..0=>FALSE));
  end;

begin
  null;
end;
