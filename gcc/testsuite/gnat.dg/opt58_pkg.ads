with System; use System;

package Opt58_Pkg is

   pragma Pure (Opt58_Pkg);

   type Small_Int is range 0 .. 255;

   type Rec is record
     D1, D2, D3, D4 : Small_Int;
   end record;
   pragma Pack (Rec);
   for Rec'Size use 32;

   function F1 (R : Rec) return Small_Int;

   function F2 (A : Address; B : Boolean) return Boolean;

end Opt58_Pkg;
