-- { dg-do run }
-- { dg-options "-O" }

with Loop_Optimization17_Pkg; use Loop_Optimization17_Pkg;

procedure Loop_Optimization17 is

  Data : Arr;

begin

  Data := (others => (I  => 0,
                      V1 => (others => 0.0),
                      V2 => (others => 0.0),
                      S  => 0.0));

  for I in Index_T'Range loop
    Object (I).V1 := F (Data (I).V1);
    Object (I).V2 := F (Data (I).V2);
  end loop;

end;
