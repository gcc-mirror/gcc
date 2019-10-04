with Inline20_G;
with Inline20_H;

package Inline20_R is

   package My_G is new Inline20_G;

   package My_H is new Inline20_H (My_G);

   procedure Log (I : Integer);

end Inline20_R;
