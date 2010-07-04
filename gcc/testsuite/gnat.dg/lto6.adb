-- { dg-do run }
-- { dg-options "-O2 -flto" { target lto } }

with Lto6_Pkg; use Lto6_Pkg;

procedure Lto6 is
  type Enum is (A, B, C, D);
  Table : array (B .. C, 1 .. 1) of F_String := (others => (others => Null_String));
begin
  Table := (others => (others => Null_String));
end;
