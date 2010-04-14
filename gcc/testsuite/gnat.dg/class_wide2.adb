-- { dg-do compile }

package body Class_Wide2 is

   procedure Initialize is
      Var_Acc : Class_Acc := new Grand_Child;
      Var     : Grand_Child'Class := Grand_Child'Class (Var_Acc.all);  -- { dg-bogus "already constrained" "" { xfail *-*-* } }

   begin
      Var := Grand_Child'Class (Var_Acc.all);
   end Initialize;

end Class_Wide2;
