-- { dg-do run }

procedure Concat_Length is
  type Byte is mod 256;
  for Byte'Size use 8;
  type Block is array(Byte range <>) of Integer;

  C0: Block(1..7) := (others => 0);
  C1: Block(8..255) := (others => 0);
  C2: Block := C0 & C1;
begin
   if C2'Length /= 255 then
      raise Program_Error;
   end if;
end;
