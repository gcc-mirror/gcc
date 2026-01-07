-- { dg-do compile }

with Ada.Containers.Indefinite_Vectors;

generic
package Class_Wide1 is

   type R is tagged record
      X : Integer;
   end record;

   package V is new Ada.Containers.Indefinite_Vectors (Positive, R);
   package V_CW is new Ada.Containers.Indefinite_Vectors (Positive, R'Class);

   function F1 (VV : V.Vector) return Integer is (VV (1).X);
   function F2 (VV : V_CW.Vector) return Integer is (VV (1).Element.X);
   function F3 (VV : V_CW.Vector) return Integer is (VV (1).X);

end Class_Wide1;
