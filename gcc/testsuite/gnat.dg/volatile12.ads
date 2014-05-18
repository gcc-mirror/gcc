package Volatile12 is

   type Arr is array (Integer range <>) of Integer with Volatile;

   procedure Proc (A : Arr);

end Volatile12;
