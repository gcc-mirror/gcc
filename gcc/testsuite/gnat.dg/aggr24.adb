-- { dg-do run }

with Aggr24_Pkg; use Aggr24_Pkg;

procedure Aggr24 is
  V : Rec;
begin
  V.S := "Hello";
  Init (V);
  if V.S /= "Hello" then
    raise Program_Error;
  end if;
end;
