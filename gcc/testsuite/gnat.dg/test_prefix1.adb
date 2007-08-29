-- { dg-do run }

with prefix1; use prefix1;
procedure test_prefix1 is
   Val : Natural;
   Obj  : T;
--
begin
   for J in Obj.Func'Range loop
      Val := Obj.Func (J);
      if Val /= 2 ** J then
         raise Program_Error;
      end if;
   end loop;
end test_prefix1;
