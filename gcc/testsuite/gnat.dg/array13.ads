package Array13 is

   Max : Natural := 1;

   type Arr is array (Natural range 0..Max) of Natural;

   type T is record
      A : Arr := (others => 0);
   end record;

   procedure Foo;

end Array13;
