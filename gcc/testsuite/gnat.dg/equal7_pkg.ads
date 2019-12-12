with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Finalization;      use Ada.Finalization;
package Equal7_Pkg is

   type Editor_Location is abstract new Controlled with null record;
   Nil_Editor_Location : constant Editor_Location'Class;

   function F (X : Integer) return Unbounded_String;
   function F (X : Integer) return String;

private
   type Dummy_Editor_Location is new Editor_Location with null record;

   Nil_Editor_Location : constant Editor_Location'Class :=
     Dummy_Editor_Location'(Controlled with null record);
end;
