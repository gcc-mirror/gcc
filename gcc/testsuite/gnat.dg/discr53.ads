with Discr53_Pkg;

package Discr53 is

   type Rec (D : Boolean := False) is record
      case D is
         when True  => S : String (1 .. Discr53_Pkg.Max);
         when False => null;
      end case;
   end record;

   function F return Rec;

   procedure Proc;

end Discr53;
