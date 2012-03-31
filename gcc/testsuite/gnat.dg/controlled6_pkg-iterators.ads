with Ada.Finalization;

generic

   I : Integer;

package Controlled6_Pkg.Iterators is

   type Iterator_Type is new Ada.Finalization.Controlled with record
      Current : Node_Access_Type;
   end record;

   function Find return Iterator_Type;

   function Current (Iterator : in Iterator_Type) return T;
   pragma Inline (Current);

   procedure Find_Next (Iterator : in out Iterator_Type);

   function Is_Null (Iterator : in Iterator_Type) return Boolean;

end Controlled6_Pkg.Iterators;
