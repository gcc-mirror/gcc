-- { dg-do compile }

package body Rep_Clause2 is

   procedure Assign (From : Data; Offset : Positive; I : Index; To : out Bit_Array) is
   begin
     To (Offset .. Offset + 7) := Bit_Array (Conv (From.D(I).S.N));
   end;

end Rep_Clause2;
