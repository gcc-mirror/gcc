-- { dg-do compile }

with Array17_Pkg; use Array17_Pkg;

procedure Array17 is
   X : aliased Varray := (1 .. 8 => 1.0);
   Y : Varray (1 .. 8) := (others => -1.0);
   R : Varray (1 .. 8);
begin
   R (1 .. 4) := Y (1 .. 4) + X (1 .. 4);
end;
