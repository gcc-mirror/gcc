with Inline20_G;

generic
   with package Msg is new Inline20_G (<>);
package Inline20_H is

   generic
      type T is private;
      with function Image (Data : T) return String;
   package Nested_H is
      package My_Nested_G is new Msg.Nested_G (T);
      function F return Integer renames My_Nested_G.F;
   end Nested_H;

end Inline20_H;
