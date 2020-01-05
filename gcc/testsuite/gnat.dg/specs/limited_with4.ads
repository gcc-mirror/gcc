-- { dg-do compile }
-- { dg-options "-gnatc" }

with Ada.Containers.Vectors;
with Limited_With4_Pkg;

package Limited_With4 is

   type Object is tagged private;
   type Object_Ref is access all Object;
   type Class_Ref is access all Object'Class;

   package Vec is new Ada.Containers.Vectors
     (Positive, Limited_With4_Pkg.Object_Ref,Limited_With4_Pkg ."=");
   subtype Vector is Vec.Vector;

private

   type Object is tagged record
      V : Vector;
   end record;

end Limited_With4;
