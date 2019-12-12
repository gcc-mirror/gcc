----
with Ada.Containers.Indefinite_Ordered_Maps;

package Predicate2.Project.Typ.Set is

   --  The type names must not be case-sensitive

   package Set is new Ada.Containers.Indefinite_Ordered_Maps
     (Name_Type, Object, "<");

   subtype Object is Set.Map;

end Predicate2.Project.Typ.Set;
