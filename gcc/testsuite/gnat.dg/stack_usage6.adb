-- { dg-do compile }
-- { dg-options "-Wstack-usage=512" }

with Stack_Usage6_Pkg; use Stack_Usage6_Pkg;

procedure Stack_Usage6 (I : Index_Type) is
   R : constant Rec := A (I);
begin
   if R.D then
     raise Program_Error;
   end if;
end;
