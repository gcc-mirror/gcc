generic

  type T is (<>);
  with function MAX_ADD(X : T; I : INTEGER) return T;

package Discr16_G is

  LO : T := T'val(T'pos(T'first));
  HI : T := T'val(T'pos(MAX_ADD(LO, 15)));

  type A2 is array(T range <>) of T;

  type R2(D : T) is
  record
    C : A2(LO..D);
  end record;

end;
