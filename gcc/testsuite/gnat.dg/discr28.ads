with Discr28_Pkg;

package Discr28 is

   type Enum is (One, Two);

   type Rec (D : Enum := One) is record
      case D is
         when One => null;
         when Two => S : String (1 .. Discr28_Pkg.N);
      end case;
   end record;

   Default_Rec : constant Rec := (D => One);

   procedure Proc1;
   procedure Proc2;

end Discr28;
