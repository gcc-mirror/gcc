-- { dg-do run }

procedure Interface_Conv is
   package Pkg is
      type I1 is interface;
      procedure Prim (X : I1) is null;
      type I2 is interface;
      procedure Prim (X : I2) is null;
      type DT is new I1 and I2 with null record;
   end Pkg;
   use Pkg;
   Obj  : DT;
   CW_3 : I2'Class := Obj;
   CW_5 : I1'Class := I1'Class (CW_3);  --  test
begin
   null;
end;
