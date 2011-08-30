-- { dg-do compile }

with Aggr3_Pkg; use Aggr3_Pkg;

package Aggr3 is

   type Enum is (One);

   type R (D : Enum := One) is
   record
      case D is
        when One => The_T : T; 
      end case;
   end record;

   My_R : R := (D => One, The_T => My_T);

end Aggr3;
