generic
   type T_horizontal is new float;

-- Declaration of types, constants, and common functions on 3D vectors.
-- Corresponds to PVS theory vectors/vectors_2D
package Aspect1_Vectors_2D is

   -- A 2D vector, represented by an x and a y coordinate.
   type Vect2 is record
      x: T_horizontal;
      y: T_horizontal;
   end record;

   subtype Nz_vect2 is Vect2
     with Predicate => (Nz_vect2.x /= 0.0 and then Nz_Vect2.y /= 0.0);
end Aspect1_Vectors_2D;
