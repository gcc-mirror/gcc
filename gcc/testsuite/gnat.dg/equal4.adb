--  { dg-do compile }

package body Equal4 is
   procedure Compare (Obj : Equal4_Full_Selector_Filter.Object_T) is
      use type Equal4_Full_Selector_Filter.Object_T;

   begin
      if Obj = Equal4_Full_Selector_Filter.True then
         null;
      end if;
   end Compare;
end Equal4;
