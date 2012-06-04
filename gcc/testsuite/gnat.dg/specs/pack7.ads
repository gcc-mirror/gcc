-- { dg-do compile }

package Pack7 is

   type R (D : Natural) is record
      S : String (1 .. D);
      N : Natural;
      B : Boolean;
   end record;
   for R'Alignment use 4;
   pragma Pack (R);

end Pack7;
