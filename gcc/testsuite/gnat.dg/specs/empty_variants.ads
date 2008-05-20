--  { dg-do compile }
--  { dg-options "-gnatdF" }

package Empty_Variants is
   
   type Rec (D : Integer := 1) is record
      case D is
         when 1 =>
            I : Integer;
         when 2 .. 5 =>
            J : Integer;
            K : Integer;
         when 6 =>
            null;
         when 7 .. 8 =>
            null;
         when others =>
            L : Integer;
            M : Integer;
            N : Integer;
      end case;
   end record;
   
   R : Rec;
   
   I : Integer := R.I;
   J : Integer := R.J;
   K : Integer := R.K;
   L : Integer := R.L;
   M : Integer := R.L;

end Empty_Variants;
