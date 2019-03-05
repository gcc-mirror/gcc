--  { dg-do compile }
--  { dg-options "-gnat95" }

with Overload2_P; use Overload2_P;
with text_io; use text_io;
procedure overload2 is
  this, that: t;
  yes : boolean := this /= that;
begin
  if not yes then
     put_line ("FAILED");
  end if;
end;
