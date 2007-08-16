--  { dg-do compile }

with deref2;
procedure deref3 is
   Obj : aliased deref2.NT;
begin
   deref2.PT_View (Obj'Access).Op;
   Obj.PT_View.all.Op;
   Obj.PT_View.Op;
end;
