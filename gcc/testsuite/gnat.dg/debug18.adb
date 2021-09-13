-- { dg-do compile }
-- { dg-skip-if "No Dwarf" { { hppa*-*-hpux* } && { ! lp64 } } }
-- { dg-options "-cargs -O0 -g -dA -fgnat-encodings=minimal -margs" }

procedure Debug18 is

   procedure Check (Size : Integer) is
      type Bit_Array_Type is array (1 .. Size) of boolean;
      pragma Pack (Bit_Array_Type);

      Bits : Bit_Array_Type := (others => False);
   begin
      Bits (Bits'First) := True;
   end;
  
begin
   Check (Size => 9);
end;

-- { dg-final { scan-assembler-not "DW_AT_lower_bound" } }
