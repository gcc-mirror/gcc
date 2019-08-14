--  { dg-do compile }

procedure Discr57 is

   type T1(Scalar : Boolean) is abstract tagged null record;

   subtype S1 is T1 (Scalar => False);

   type T2(Lower_Bound : Natural) is new
     S1 with null record;

   Obj : constant T2 :=
       (Lower_Bound => 123);

begin
   null;
end Discr57;
