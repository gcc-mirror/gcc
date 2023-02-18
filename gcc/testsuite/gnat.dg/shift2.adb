-- { dg-do compile }

with Interfaces; use Interfaces;

function Shift2 (V : Unsigned_32) return Unsigned_32 is
begin
  return Shift_Left (V, (case V is when 0 => 1, when others => 0));
end;
