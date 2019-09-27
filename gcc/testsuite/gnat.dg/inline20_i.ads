with Inline20_R;

generic
package Inline20_I is

   type Rec is null record;

   generic
   package Generic_IO is

      function Image (Quote : Rec) return String;

      package My_Nested_H is new Inline20_R.My_H.Nested_H (Rec, Image);

      function F return Integer renames My_Nested_H.F;

   end Generic_IO;

end Inline20_I;