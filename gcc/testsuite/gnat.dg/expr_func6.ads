pragma Assertion_Policy (Check);

package Expr_Func6 is

   type Monolean is (Nottrue);

   function Basic_Function return Monolean;
   function Fancy_Function_With_Contract return Boolean
     with Pre => Basic_Function = Nottrue;

   function Fancy_Function_With_Contract return Boolean is (False);

   function Basic_Function return Monolean is (Nottrue);

   procedure Dummy;

end Expr_Func6;
