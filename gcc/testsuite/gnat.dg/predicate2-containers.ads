----
with Ada.Containers.Indefinite_Vectors;

package Predicate2.Containers is

   subtype Count_Type is Ada.Containers.Count_Type;

   package Value_Type_List is
     new Ada.Containers.Indefinite_Vectors (Positive, Value_Type);

   subtype Value_List is Value_Type_List.Vector;

end Predicate2.Containers;
