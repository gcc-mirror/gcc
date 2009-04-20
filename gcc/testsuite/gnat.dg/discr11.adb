-- { dg-do compile }

package body Discr11 is
   function Create return DT_2 is
   begin
      return DT_2'(DT_1'(Create) with More => 1234);
   end;
end Discr11;

