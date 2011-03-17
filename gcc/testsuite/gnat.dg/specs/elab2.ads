-- { dg-do compile }

with Elab2_Pkg; use Elab2_Pkg;

package Elab2 is

   type Num is (One, Two);

   type Rec2 (D : Index_Type := 0) is record
      Data : Rec1(D);
   end record;

   type Rec3 (D : Num) is record
      case D is
         when One => R : Rec2;
         when others => null;
      end case;
   end record;

end Elab2;
