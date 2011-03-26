limited with Limited_With2_Pkg2;

package Limited_With2_Pkg1 is

   type Rec2 is record
      F : access Limited_With2_Pkg2.Rec3;
   end record;

end Limited_With2_Pkg1;
