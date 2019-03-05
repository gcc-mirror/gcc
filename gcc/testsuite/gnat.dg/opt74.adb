-- { dg-do run }
-- { dg-options "-O2" }

with Opt74_Pkg; use Opt74_Pkg;

procedure Opt74 is
   Index, Found : Integer;
begin
   Proc (Found, Index);
   if Found = 1 then
      raise Program_Error;
   end if;
end;
