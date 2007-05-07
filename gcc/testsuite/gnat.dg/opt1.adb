-- { dg-do compile }
-- { dg-options "-O -gnatws" }

package body Opt1 is

   function De_Linear_Index
     (Index       : Natural;
      D           : Natural;
      Ind_Lengths : Dimention_Length)
      return        Dimension_Indexes
   is
      Len     : Natural := 1;
      Tmp_Ind : Natural := Index;
      Tmp_Res : Natural;
      Result  : Dimension_Indexes (1 .. D);
   begin
      for J in 1 .. D loop
         Len := Len * Ind_Lengths (J);
      end loop;

      for J in Result'Range loop
         Result (J) := Tmp_Res;
         Tmp_Ind := Tmp_Ind - Len * (Result (J) - 1);
      end loop;

      return Result;
   end;

end Opt1;
