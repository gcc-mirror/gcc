with Ada.Containers.Indefinite_Vectors;

generic
package Aggr34_Pkg3 is
   package Config_Data_Paths is new
     Ada.Containers.Indefinite_Vectors (Positive, String);
   procedure Set (Path : Config_Data_Paths.Vector) is null;
end Aggr34_Pkg3;
