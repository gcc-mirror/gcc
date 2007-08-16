--  { dg-do run }

with dispatch2_p; use dispatch2_p;
procedure dispatch2 is
   Obj : Object_Ptr := new Object;
begin
   if Obj.Get_Ptr /= Obj.Impl_Of then
      raise Program_Error;
   end if;
end;
