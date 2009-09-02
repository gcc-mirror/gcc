package body Misaligned_Param_Pkg is

  type IP is access all Integer;

  function Channel_Eth (Kind : IP) return Integer;
  pragma Export (Ada, Channel_Eth, "channel_eth");

  function Channel_Eth (Kind : IP) return Integer is
  begin
    Kind.all := 111;
    return 0;
  end;

end Misaligned_Param_Pkg;
