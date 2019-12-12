with Ada.Containers.Functional_Vectors;
with Ada.Containers; use Ada.Containers;

generic
   type Element_Type (<>) is private;
   type Element_Model (<>) is private;
   with function Model (X : Element_Type) return Element_Model is <>;
   with function Copy (X : Element_Type) return Element_Type is <>;
package Aspect2 with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);

   type Vector is private;

   function Length (V : Vector) return Natural;

   procedure Foo;

private
   type Element_Access is access Element_Type;
   type Element_Array is array (Positive range <>) of Element_Access with
     Dynamic_Predicate => Element_Array'First = 1;
   type Element_Array_Access is access Element_Array;
   type Vector is record
      Top     : Natural := 0;
      Content : Element_Array_Access;
   end record;

   function Length (V : Vector) return Natural is
     (V.Top);
end Aspect2;
