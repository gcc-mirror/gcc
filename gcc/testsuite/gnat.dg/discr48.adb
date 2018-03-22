-- { dg-do compile }

with Discr48_Pkg; use Discr48_Pkg;

function Discr48 return Rec_Access is
   C : constant Rec := (Count => 1, Seps => (1 .. 0 => Null_XString));
begin
   return new Rec'(C);
end;
