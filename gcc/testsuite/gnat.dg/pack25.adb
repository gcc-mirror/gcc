--  { dg-do run }
procedure Pack25 is

   type Bit is ('0', '1');
   type Bit_Array is array (Natural range <>) of Bit;
   pragma Pack (Bit_Array);

   procedure Test (Bits : Bit_Array; Size : Natural) is
   begin
      if Bits (0 .. Size - 1)'Size /= Size then
         raise Program_Error;
      end if;
   end;

   A : Bit_Array (0 .. 127) := (others => '1');

begin
   for X in A'First .. A'Last + 1 loop
      Test (A, X);
   end loop;
end;