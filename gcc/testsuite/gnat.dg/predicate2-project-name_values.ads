
----
with Predicate2.Containers;
with Predicate2.Project.Registry.Attribute;
with Predicate2.Source_Reference;

private with Ada.Strings.Unbounded;

package Predicate2.Project.Name_Values is

   use type Containers.Count_Type;
   use all type Registry.Attribute.Value_Kind;

   type Object is new Source_Reference.Object with private;

   Undefined : constant Object;

   subtype Value_Kind is Registry.Attribute.Value_Kind;

   function Kind (Self : Object'Class) return Registry.Attribute.Value_Kind
     with Pre => Object (Self) /= Undefined;
   --  Returns the Kind for the Name/Values pair object

private

   use Ada.Strings.Unbounded;

   type Object is new Source_Reference.Object with record
      Kind   : Registry.Attribute.Value_Kind := List;
      Name   : Unbounded_String;
      Values : Containers.Value_List;
   end record;

   Undefined : constant Object :=
                 Object'(Source_Reference.Object with others => <>);

end Predicate2.Project.Name_Values;
