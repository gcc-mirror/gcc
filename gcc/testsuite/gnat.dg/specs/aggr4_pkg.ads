-- { dg-excess-errors "cannot generate code" }

package Aggr4_Pkg is

   function F return Integer;

   type Rec1 is tagged record
      I : Integer;
   end record;

   Zero : constant Rec1 := (I => F);

   type Enum is (One, Two);

   type Rec2 (D : Enum := One) is record
      case D is
         when One => Value : Rec1;
         when others => null;
      end case;
   end record;

   type Rec3 is record
      Data : Rec2;
   end record;

end Aggr4_Pkg;
