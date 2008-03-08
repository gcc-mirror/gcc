-- { dg-do compile }
-- { dg-options "-O -Wall" }

function uninit_func (A, B : Boolean) return Boolean is
   C : Boolean; -- { dg-warning "may be used uninitialized" }
begin
   if A then
      C := False;
   elsif B then
      C := True;
   end if;
   return C;
end;
