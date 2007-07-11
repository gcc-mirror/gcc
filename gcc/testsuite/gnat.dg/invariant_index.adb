-- { dg-do compile }
-- { dg-options "-O -gnatp" }

package body Invariant_Index is

   procedure Proc (S : String) is
      N : constant Integer := S'Length;
   begin
      Name_Buffer (1 + N .. Name_Len + N) := Name_Buffer (1 .. Name_Len);
      Name_Buffer (1 .. N) := S;
      Name_Len := Name_Len + N;
   end;

end Invariant_Index;
