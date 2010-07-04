-- { dg-do compile }

with Prot2_Pkg1;
with Prot2_Pkg2;

package body Prot2 is

   type A is array (1 .. Prot2_Pkg1.Num) of Integer;

   type E is (One, Two);

   type Rec (D : E := One) is record
      case D is
         when One => L : A;
         when Two => null;
      end case;
   end record;

   package My_Pkg2 is new Prot2_Pkg2 (Rec);

   procedure Dummy is begin null; end;

end Prot2;
