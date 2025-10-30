-- { dg-do compile }

package Generic_Inst7 is

   function F return Integer is (0);

   generic
     with function Foo return Integer;
   package P is
     type Color is (Foo);
   end P;

   package My_P is new P (F);

   I : Integer := My_P.Foo; -- { dg-error "expected type|found type" }

end Generic_Inst7;
