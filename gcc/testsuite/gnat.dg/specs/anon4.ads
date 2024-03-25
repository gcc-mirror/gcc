-- { dg-do compile }

package Anon4 is
   subtype I is Integer;
   A : access I;

   Acc : access Integer;

   function F return Boolean is (A = Acc);
end Anon4;
