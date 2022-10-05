-- { dg-do run }
-- { dg-options "-O2 -flto" { target lto } }

with Ada.Streams; use Ada.Streams;
with Lto26_Pkg1; use Lto26_Pkg1;

procedure Lto26 is
  R : Rec;
begin
  for I in 1 .. 10 loop
    Set (R, (7, 0, 84, Stream_Element (I), 0, 0, 0), 1);
  end loop;
end;
