package Pack4 is

   type Buffer is array (Natural range <>) of Boolean;

   type Root (Size : Natural) is tagged record
      Data : Buffer (1..Size);
   end record;
   pragma Pack (Root);

   type Derived is new Root with null record;

end Pack4;
