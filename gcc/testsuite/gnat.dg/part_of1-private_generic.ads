private generic
   Count : Integer := 4;
package Part_Of1.Private_Generic
with
   Abstract_State => State
is

   subtype Range_Type is Integer range 1 .. Count;

   function Get (I : Range_Type) return Integer;

end Part_Of1.Private_Generic;
