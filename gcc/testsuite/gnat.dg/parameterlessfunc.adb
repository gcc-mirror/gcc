--  { dg-do compile }

procedure parameterlessfunc is
  type Byte is mod 256;
  type Byte_Array is array(Byte range <>) of Byte;
  subtype Index is Byte range 0..7;
  subtype Small_Array is Byte_Array(Index);
  
  function F return Byte_Array is
  begin
    return (0..255=>0);
  end F;
  
  B5: Small_Array := F(Index);
begin
  null;
end parameterlessfunc;
