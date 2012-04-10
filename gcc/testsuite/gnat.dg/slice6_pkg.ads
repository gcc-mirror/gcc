package Slice6_Pkg is

  subtype LENGTH_RANGE is SHORT_INTEGER range 0 .. 8184;

  type T_BLOCK is array (SHORT_INTEGER range <>) of SHORT_SHORT_INTEGER;
  for T_BLOCK'alignment use 4;

  type T_MSG (V_LENGTH : LENGTH_RANGE := 0) is
    record
      HEADER : Integer;
      DATAS  : T_BLOCK (1 .. V_LENGTH) := (others => 0);
    end record;
  for T_MSG'alignment use 4;

end Slice6_Pkg;
