-- { dg-do compile }
-- { dg-options "-gnatws" }

procedure Bit_Packed_Array2 is

  type Bit_Array is array (integer range <>) of Boolean;
  pragma Pack(Bit_Array);

  b1  : Bit_Array(1..64);
  b2  : Bit_array(1..64);
  res : Bit_array(1..64);

begin

  if (not((not b1) or (not b2))) /= res then 
    null;
  end if;

end;
