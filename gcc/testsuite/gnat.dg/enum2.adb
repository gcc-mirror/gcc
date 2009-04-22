-- { dg-do run }
-- { dg-options "-gnat05 -O2" }

with Enum2_Pkg; use Enum2_Pkg;

procedure Enum2 is
  type Enum is (A, B, C, D);
  Table : array (B .. C, 1 .. 1) of F_String := (others => (others => Null_String));
begin
  Table := (others => (others => Null_String));
end;
