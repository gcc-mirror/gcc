-- { dg-do run }

procedure iprot_test is
      type T1 is tagged  null record;
      package PP is
         protected type P is
            procedure S (X : T1'Class);
         private
            R2 : access T1'Class;
         end P;
      end PP;
      package body PP is
         protected body P is
            procedure S (X : T1'Class) is
            begin
               R2 := new T1'Class'(X);
               if R2 /= null then
                  null;
               end if;
            end S;
         end P;
      end PP;
      use PP;
      Prot : P;
      procedure Proc is
         type T2 is new T1 with null record;
         X2 : T2;
      begin
         Prot.S (X2);
      end Proc;
begin
   Proc;
exception
   when Program_Error => null;
end iprot_test;
