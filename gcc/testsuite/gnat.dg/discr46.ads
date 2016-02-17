package Discr46 is

   type Enum is (One, Two, Three);
   for Enum use (One => 1, Two => 2, Three => 3);

   type Rec1 (D : Boolean := False) is record
      case D is
         when False => null;
         when True => T : Integer;
      end case;
   end record;

   type Rec2 is record
      R : Rec1;
      C : Character;
   end record;

   type Arr is array (Enum) of Rec2;

   A : Arr; 

   function F (Id : Enum) return Integer;  

end Discr46;
