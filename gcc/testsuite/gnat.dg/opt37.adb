-- { dg-do compile }
-- { dg-options "-O2 -gnato -fdump-tree-optimized" }

package body Opt37 is

   function To_Unchecked (Bits : T_Bit_Array) return Unsigned32 is
      Value : Unsigned32 := 0;
   begin
      for I in Bits'Range loop
         Value := Value * 2 + Unsigned32 (Bits(I));
      end loop;
      return Value;
   end;

   function To_Scalar (Bits : T_Bit_Array) return Positive is
      Tmp   : Unsigned32;
      Value : Positive;
   begin
      Tmp := To_Unchecked (Bits);
      if Tmp in 0 .. Unsigned32 (Positive'last) then
         Value := Positive (Tmp);
      else
         Value := -Positive (Unsigned32'last - Tmp);
         if Value > Positive'first then
            Value := Value - 1;
         else
            raise Program_Error;
         end if;
      end if;
      return Value;
   end;

   function Func (Bit_Array : T_Bit_Array;
                  Bit_Index : T_Bit_Index) return Positive is
   begin
      return To_Scalar (Bit_Array (Bit_Index .. Bit_Index + 1));
   end;

end Opt37;

-- { dg-final { scan-tree-dump-not "alloca" "optimized" } }
