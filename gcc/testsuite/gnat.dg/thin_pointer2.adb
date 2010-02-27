-- PR ada/42253
-- Testcase by Duncan Sands <baldrick@gcc.gnu.org>

-- { dg-do run }

with Thin_Pointer2_Pkg; use Thin_Pointer2_Pkg;

procedure Thin_Pointer2 is
begin
   if F /= '*' then
      raise Program_Error;
   end if;
end;
