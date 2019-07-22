--  { dg-do compile }

procedure Class_Wide5 is
   type B is interface;
   type B_Child is new B with null record;
   type B_Ptr is access B'Class;

   procedure P (Obj : B_Ptr) is begin null; end;
begin
   P (new B_child);  -- Test
end Class_Wide5;
