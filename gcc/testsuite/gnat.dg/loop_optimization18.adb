-- { dg-do compile }
-- { dg-options "-O3" }

package body Loop_Optimization18 is

   procedure Proc (Message : Byte_Array_Type) is

      R : Rec (Conv (Message));

   begin
      for Division in 1 .. R.UB loop
         R.L (Division) := 0;
      end loop;
  end;

end Loop_Optimization18;
