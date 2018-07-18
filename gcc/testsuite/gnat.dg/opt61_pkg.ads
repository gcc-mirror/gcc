with Interfaces;

package Opt61_Pkg is

   subtype Int64 is Interfaces.Integer_64;

   procedure Double_Divide
     (X, Y, Z : Int64;
      Q, R    : out Int64;
      Round   : Boolean);

end Opt61_Pkg;
