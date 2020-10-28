-- { dg-do compile }

with Addr16_Pkg; use Addr16_Pkg;

procedure Addr16 (R : Rec) is

  pragma Unsuppress (Alignment_Check);

  B : Integer;
  for B'Address use R.A'Address;

begin
  null;
end;
