-- { dg-compile }

procedure Inline22 (L, U : Integer) is

  type Arr is array (Integer range L .. U) of Boolean;

  function Get_Zero return Arr;
  pragma Inline_Always (Get_Zero);

  function Get_Zero return Arr is
  begin
    return (others => False);
  end;

  A : Arr;

begin
  A := Get_Zero;
end;
