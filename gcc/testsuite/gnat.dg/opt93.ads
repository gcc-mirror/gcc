package Opt93 is

  type Arr is array (Natural range <>) of Short_Integer;

  type Rec (D1, D2 : Natural) is record
    S : String (1 .. D1);
    A : Arr (1 .. D2);
  end record;

  type T is access Rec;

  function Contains_Zero (Obj : T) return Boolean;

end Opt93;
