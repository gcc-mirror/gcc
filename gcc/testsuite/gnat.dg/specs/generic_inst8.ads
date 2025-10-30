-- { dg-do compile }

package Generic_Inst8 is

   function F return Integer is (0);

   generic
     with function Foo return Integer;
   package P is
     type Color1 is (Foo);
     type Color2 is (Foo);
   end P;

   package My_P is new P (F);

   I : Integer := My_P.Foo; -- { dg-error "no visible interpretation|use" }

end Generic_Inst8;
