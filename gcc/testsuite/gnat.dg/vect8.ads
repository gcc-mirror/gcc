-- { dg-do compile }

package Vect8 is

   type Vec is array (1 .. 2) of Long_Float;
   pragma Machine_Attribute (Vec, "vector_type");

   function Foo (V : Vec) return Vec;

end Vect8;
