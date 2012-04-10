package Aggr15 is

  type T is tagged record
    I : Integer;
  end record;

  type DATA_T is record
    D : T;
  end record;

  type ALL_DATA_T is array (1..2, 1..2) of DATA_T;

  function ALL_CREATE return ALL_DATA_T;

end Aggr15;
