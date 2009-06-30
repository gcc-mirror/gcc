-- { dg-do compile }

with Discr16_G;
with Discr16_Cont; use Discr16_Cont;

procedure Discr16 is

  generic
    type T is (<>);
  function MAX_ADD_G(X : T; I : INTEGER) return T;

  function MAX_ADD_G(X : T; I : INTEGER) return T is
  begin
    return T'val(T'pos(X) + LONG_INTEGER(I));
  end;

  function MAX_ADD is new MAX_ADD_G(ES6A);

  package P is new Discr16_G(ES6A, MAX_ADD);

begin
  null;
end;
