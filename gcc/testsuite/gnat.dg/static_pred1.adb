--  { dg-do compile }

package body Static_Pred1 is

   type Enum_Type is (A, B, C);

   subtype Enum_Subrange is Enum_Type with Static_Predicate =>
     Enum_Subrange in A | C;

   function "not" (Kind : Enum_Subrange) return Enum_Subrange is
     (case Kind is
      when A => C,
      when C => A);

   procedure Dummy (Value : T) is
      IK : Enum_Subrange := not A;
   begin
      null;
   end Dummy;

end Static_Pred1;
