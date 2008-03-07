-- { dg-do run }

procedure Pack3 is

  type U32 is mod 2 ** 32;

  type Key is record
    Value : U32;
    Valid : Boolean;
  end record;

  type Key_Buffer is record
    Current, Latch : Key;
  end record;

  type Block is record
    Keys  : Key_Buffer;
    Stamp : U32;
  end record;
  pragma Pack (Block);

  My_Block : Block;
  My_Stamp : constant := 16#01234567#;

begin
  My_Block.Stamp := My_Stamp;
  My_Block.Keys.Latch := My_Block.Keys.Current;
  if My_Block.Stamp /= My_Stamp then
    raise Program_Error;
  end if;
end;
