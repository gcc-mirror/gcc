-- { dg-do compile }
-- { dg-options "-O -Wall" }

function uninit_func (A, B : Boolean) return Boolean is
   C : Boolean;
begin
   if A then
      C := False;
   elsif B then
      C := True;
   end if;
   return C; -- { dg-warning "may be used uninitialized" }
end;
