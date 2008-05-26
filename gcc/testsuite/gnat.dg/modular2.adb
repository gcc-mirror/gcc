--  { dg-do run }

procedure modular2 is
   type x is mod 2 ** 64;
   r : x := x'last;
begin
   r := r + 1;
end;
