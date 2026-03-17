-- { dg-do compile }

procedure Access12 is

   type Rec (Element : access Integer) is null record;

   function Make_Rec (X : access Integer) return Rec is (Element => X);

   type Acc is access all Integer;

   A : Acc;

begin
   for I in 1 .. 10 loop
      declare
         X : aliased Integer;
         R : Rec := Make_Rec (X'Access);
      begin
         if I = 1 then
            X := 0;
         end if;
         A := R.Element.all'Access; -- { dg-error "non-local pointer" }
         X := I;
      end;
   end loop;
end;
