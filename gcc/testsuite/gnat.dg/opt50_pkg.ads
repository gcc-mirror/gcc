package Opt50_Pkg is

   type Enum is (One, Two, Three);
   for Enum'Size use 16;

   type Enum_Boolean_Array is array (Enum range <>) of Boolean;

   procedure Get (Kind : String; Result : out Enum; Success : out Boolean);

   procedure Set (A : Enum_Boolean_Array);

end Opt50_Pkg;
