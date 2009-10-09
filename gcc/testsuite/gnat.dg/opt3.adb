-- { dg-do compile }
-- { dg-options "-O3" }

with Opt3_Pkg; use Opt3_Pkg;

procedure Opt3 is
  type Buffer_Type is array (Integer range <> ) of Short_Integer;
  B : Buffer_Type (1 .. 256) := (others => 0);
begin
  F (B(1));
end;
