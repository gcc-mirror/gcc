generic

   Max_Length : Positive;

package Discr59_Pkg2 is

   type Token_Base_Type (Most : Natural) is record
      Text : String (1 .. Most) := (others => ' ');
      Last : Natural            := 0;
      Used : Natural            := 0;
   end record;

   type Token_Type is new Token_Base_Type (Max_Length);

end Discr59_Pkg2;
