
private with Ada.Strings.Unbounded;

package Predicate2.Source_Reference is

   type Object is tagged private;

   subtype Source_Reference is Object;

   function "<" (Left, Right : Object) return Boolean;

   Undefined : constant Object;

private

   use Ada.Strings.Unbounded;

   type Object is tagged record
      Line     : Natural;
      Column   : Natural;
      Filename : Unbounded_String;
   end record
     with Dynamic_Predicate => Filename /= Null_Unbounded_String;

   function "<" (Left, Right : Object) return Boolean is
     (Left.Filename < Right.Filename
       or else
      (Left.Filename = Right.Filename and then Left.Line < Right.Line));

   Undefined : constant Object :=
                 (0, 0, To_Unbounded_String ("@"));

end Predicate2.Source_Reference;
