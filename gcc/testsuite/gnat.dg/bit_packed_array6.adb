-- { dg-do run }
-- { dg-options "-O2 -gnata -gnatVa" }

with Bit_Packed_Array6_Pkg; use Bit_Packed_Array6_Pkg;

procedure Bit_Packed_Array6 is
  B : constant Boolean := Everywhere (K_Configuration);
begin
  null;
end;
