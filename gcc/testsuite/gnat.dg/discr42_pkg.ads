package Discr42_Pkg is

   type Rec (D : Boolean := False) is record
      case D is
         when True  => N : Natural;
         when False => null;
      end case;
   end record;

   function F (Pos : in out Natural) return Rec;

end Discr42_Pkg;
