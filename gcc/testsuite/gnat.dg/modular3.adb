-- { dg-do run }

with Modular3_Pkg; use Modular3_Pkg;

procedure Modular3 is

  function F1 (A : Int16_T) return Int16_T is
  begin
    return A + 128;
  end;

  function F2 (B : Mod16_T) return Mod16_T is
  begin
    return B + 128;
  end;

  A : Int16_T := 16384;
  B : Mod16_T := 65504;

begin

  A := F1 (A);
  if A /= 16512 then
    raise Program_Error;
  end if;

  B := F2 (B);
  if B /= 96 then
    raise Program_Error;
  end if;

end Modular3;
