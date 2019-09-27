generic
package Inline21_H is

   generic
   package Nested_G is
      procedure Proc;
      pragma Inline (Proc);
   end Nested_G;

end Inline21_H;
