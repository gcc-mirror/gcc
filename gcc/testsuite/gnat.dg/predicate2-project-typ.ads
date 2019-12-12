----
with Predicate2.Project.Name_Values;

private with Predicate2.Project.Registry.Attribute;

package Predicate2.Project.Typ is

   type Object is new Name_Values.Object with private;

   Undefined : constant Object;

private

   use all type Predicate2.Project.Registry.Attribute.Value_Kind;

   -- ???? BUG HERE: removing the Dynamic_Predicate below will allow
   --  compilation of the unit.

   type Object is new Name_Values.Object with null record
    with Dynamic_Predicate => Object.Kind = List;

   Undefined : constant Object := (Name_Values.Undefined with null record);

end Predicate2.Project.Typ;
