package Aliasing2 is

  type Arr is Array (1..4) of Integer;
  type Ptr is access all Integer;

  A : Arr;

  function F (P : Ptr) return Integer;

end Aliasing2;
