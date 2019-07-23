procedure Rep_Clause8 is
   package Pack is
      type Root is tagged record
         Comp : Integer;
      end record;
   end Pack;
   use Pack;

   generic
      type Formal_Root is new Root with private;
   package Gen_Derived is
      type Deriv is new Formal_Root with null record
        with Size => 300; --  { dg-error "representation item not allowed for generic type" }
   end Gen_Derived;

   package Inst_Derived is new Gen_Derived (Root);
begin
   null;
end;
