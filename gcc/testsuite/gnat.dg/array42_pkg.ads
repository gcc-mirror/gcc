package Array42_Pkg is

  subtype S2  is String (1 .. 2);
  subtype S4  is String (1 .. 4);
  subtype S5  is String (1 .. 5);
  subtype S8  is String (1 .. 8);
  subtype S12 is String (1 .. 12);
  subtype S16 is String (1 .. 16);

  function LT2  (A, B : S2)  return Boolean is (A < B);
  function LT4  (A, B : S4)  return Boolean is (A < B);
  function LT8  (A, B : S8)  return Boolean is (A < B);
  function LT16 (A, B : S16) return Boolean is (A < B);

  function LT5  (A, B : S5)  return Boolean is (A < B);
  function LE5  (A, B : S5)  return Boolean is (A <= B);
  function GT5  (A, B : S5)  return Boolean is (A > B);
  function GE5  (A, B : S5)  return Boolean is (A >= B);

  function LT45 (A : S4; B : S5) return Boolean is (A < B);
  function LT54 (A : S5; B : S4) return Boolean is (A < B);

  function LT (A, B : String) return Boolean is (A < B);

end Array42_Pkg;
