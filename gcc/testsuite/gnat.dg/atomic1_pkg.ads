package Atomic1_Pkg is

   type Four_Bits is mod 2 ** 4;

   type R16 is record
      F1 : Four_Bits;
      F2 : Four_Bits;
      F3 : Four_Bits;
      F4 : Four_Bits;
   end record;
   for R16 use record
      F1 at 0 range 0  ..  3;
      F2 at 0 range 4  ..  7;
      F3 at 0 range 8  .. 11;
      F4 at 0 range 12 .. 15;
   end record;

   type R32 is record
      F1 : Four_Bits;
      F2 : Four_Bits;
      F3 : Four_Bits;
      F4 : Four_Bits;
      F5 : Four_Bits;
      F6 : Four_Bits;
      F7 : Four_Bits;
      F8 : Four_Bits;
   end record;
   for R32 use record
      F1 at 0 range 0  ..  3;
      F2 at 0 range 4  ..  7;
      F3 at 0 range 8  .. 11;
      F4 at 0 range 12 .. 15;
      F5 at 0 range 16 .. 19;
      F6 at 0 range 20 .. 23;
      F7 at 0 range 24 .. 27;
      F8 at 0 range 28 .. 31;
   end record;

   C_16 : constant R16 := (2, 3, 5, 7);
   C_32 : constant R32 := (1, 1, 2, 3, 5, 8, 13, 5);

   V_16 : R16;
   pragma Atomic (V_16);
   V_32 : R32;
   pragma Atomic (V_32);

end Atomic1_Pkg;
