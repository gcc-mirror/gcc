--  { dg-do run }

with access3; use access3;
procedure access4 is
   Obj_IT : aliased T;
   Obj_T2 : T2;
begin
   Obj_T2.Op (Obj_IT'Access);
end;
