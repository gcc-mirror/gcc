--  { dg-do run }

with Disp2_Pkg; use Disp2_Pkg;

procedure Disp2 is
   Obj : Object_Ptr := new Object;
begin
   if Obj.Get_Ptr /= Obj.Impl_Of then
      raise Program_Error;
   end if;
end;
