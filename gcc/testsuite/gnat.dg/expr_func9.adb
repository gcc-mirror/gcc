--  { dg-do compile }
--  { dg-options "-gnatws" }

procedure Expr_Func9 is

   type Root is interface;

   type Child1 is new Root with null record;

   type Child2 is new Root with record
      I2 : Integer;
   end record;

   function Create (I : Integer) return Child2 is (I2 => I);

   I : Root'Class :=
         (if False
          then Child1'(null record)
          else
           Create (1));

begin
   null;
end Expr_Func9;
