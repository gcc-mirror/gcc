-- { dg-do run }
-- { dg-options "-O -gnatp" }

with Volatile11_Pkg; use Volatile11_Pkg;

procedure Volatile11 is

      Value : Integer := 1;
      Bit1 : Boolean := false;
      pragma Volatile (Bit1);
      Bit2 : Boolean := false;
      pragma Volatile (Bit2);
      Bit3 : Boolean := false;
      pragma Volatile (Bit3);
      Bit4 : Boolean := false;
      pragma Volatile (Bit4);
      Bit5 : Boolean := false;
      pragma Volatile (Bit5);
      Bit6 : Boolean := false;
      pragma Volatile (Bit6);
      Bit7 : Boolean := false;
      pragma Volatile (Bit7);
      Bit8 : Boolean := false;
      pragma Volatile (Bit8);

begin
      Bit_Test(Input   => Value,
               Output1 => Bit1,
               Output2 => Bit2,
               Output3 => Bit3,
               Output4 => Bit4,
               Output5 => Bit5,
               Output6 => Bit6,
               Output7 => Bit7,
               Output8 => F.all);

      -- Check that F is invoked before Bit_Test
      if B /= True then
        raise Program_Error;
      end if;
end;
