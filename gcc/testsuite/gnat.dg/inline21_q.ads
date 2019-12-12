with Inline21_G;

package Inline21_Q is

   package My_G is new Inline21_G;

   package My_Nested_G is new My_G.My_H.Nested_G;

end Inline21_Q;
