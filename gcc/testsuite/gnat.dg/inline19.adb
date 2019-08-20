--  { dg-do compile }
--  { dg-options "-O2" }

package body Inline19 is

   S : String := "Hello";

   protected body P is
      function F return String is
      begin
         return Result : constant String := S do
            null;
         end return;
      end F;
   end P;

end Inline19;
