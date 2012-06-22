package Lto15 is

   type Arr is array (Positive range <>) of Integer;

   type R(Size : Positive) is record
      Data : Arr (1 .. Size);
   end record;

   function Proc (Data : Arr) return R;

end Lto15;
