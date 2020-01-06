-- { dg-do compile }
-- { dg-options "-gnatc" }

package Private1.Sub is

   package Nested is
      type T is limited private;
      function "=" (X, Y : T) return Boolean;
   private
      type T is new Private1.T;
   end Nested;

end Private1.Sub;
