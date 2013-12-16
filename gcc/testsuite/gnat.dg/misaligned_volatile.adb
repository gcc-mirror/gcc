-- { dg-do run }
-- { dg-options "-gnatp -fstrict-volatile-bitfields" }

procedure Misaligned_Volatile is

   type Byte is mod 2**8;

   type Block is record
      B : Boolean;
      V : Byte;
   end record;
   pragma Volatile (Block);
   pragma Pack (Block);
   for Block'Alignment use 1;

   type Pair is array (1 .. 2) of Block;

   P : Pair;
begin
   for K in P'Range loop
      P(K).V := 237;
   end loop;
   for K in P'Range loop
      if P(K).V /= 237 then
         raise Program_error;
      end if;
   end loop;
end;
