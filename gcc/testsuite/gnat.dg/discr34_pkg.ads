package Discr34_Pkg is

   function N return Natural;

   type Enum is (One, Two);

   type Rec (D : Enum := One) is record
      case D is
	 when One => S : String (1 .. N);
	 when Two => null;
      end case;
   end record;

   function F return Rec;

end Discr34_Pkg;
