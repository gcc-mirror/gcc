package Expr_Func7 is

   type Abstract_Food is tagged null record;
   type Abstract_Food_Access is access Abstract_Food'Class;

   type Fruit is new Abstract_Food with record
      Worm : Boolean;
   end record;

   type Bananas is tagged record
      Inside : Abstract_Food_Access;
   end record;

   function Has_Worm
     (B : Bananas) return Boolean is (Fruit (B.Inside.all).Worm);

   Cool : Bananas;

   procedure Dummy;
end Expr_Func7;
