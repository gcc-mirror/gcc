with Limited1_Inner; use Limited1_Inner;

package Limited1_Outer is
   type Outer_Type (What : Boolean) is record
      Inner : Inner_Type (What);
   end record;

   function Make_Outer return Outer_Type;
end Limited1_Outer;
