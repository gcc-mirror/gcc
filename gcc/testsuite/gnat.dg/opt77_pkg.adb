package body Opt77_Pkg is

  function Compare (S : String) return Boolean is
  begin
    return S = "Two";
  end;

  procedure Proc (S : String; N : in out Natural; To_Add : out Boolean) is
    To_Take : Boolean := False;
    To_Read : Boolean := False;
  begin
    To_Add := False;

    if S = "One" then
      To_Read := True;
      To_Take := Compare (S);
    end if;

    if To_Read and not To_Take then
      N := N + 1;
    end if;

    if To_Take then
      To_Add := True;
    end if;
  end;

end Opt77_Pkg;
