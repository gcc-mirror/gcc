with Limited_With2_Pkg1;

package Limited_With2 is

   type Rec1 is record
     F : Limited_With2_Pkg1.Rec2;
   end record;

   function Func (Val : Rec1) return Limited_With2_Pkg1.Rec2;

end Limited_With2;
