with Ada.Numerics.Discrete_Random;

package Oversize is

   subtype M1 is Integer range 1 .. 200;                    -- Won't trigger
   type R1 (D : M1 := 100) is record
      Name : String (1 .. D);
   end record;

   type M2 is new Integer range 1 .. 200;                   -- Won't trigger
   for M2'Size use 64;
   type M2S is array (M2 range <>) of Character;
   type R2 (D : M2 := 100) is record
      Name : M2S (1 .. D);
   end record;

   subtype M3 is Integer;                                   -- Will trigger
   type R3 (D : M3 := 100) -- { dg-error "may raise Storage_Error" }
   is record
      Name : String (1 .. D);
   end record;

   type M4 is new Positive;                                 -- Will trigger
   type M4S is array (M4 range <>) of Character;
   type R4 (D : M4 := 100) -- { dg-error "may raise Storage_Error" }
   is record
      Name : M4S (1 .. D);
   end record;

   type M5 is new Positive;                                 -- Will trigger
   for M5'Size use Integer'Size - 1;
   type M5S is array (M5 range <>) of Character;
   type R5 (D : M5 := 100) -- { dg-error "may raise Storage_Error" }
   is record
      Name : M5S (1 .. D);
   end record;

   subtype M6 is Integer range 1 .. (Integer'Last + 1)/2;   -- Won't trigger
   type R6 (D : M6 := 100) is record
      Name : String (1 .. D);
   end record;

   subtype M7 is Integer range 1 .. (Integer'Last + 1)/2+1; -- Will trigger
   type R7 (D : M7 := 100) -- { dg-error "may raise Storage_Error" }
   is record
      Name : String (1 .. D);
   end record;

   package P8 is new Ada.Numerics.Discrete_Random (Natural);
   G8 : P8.Generator;
   subtype M8 is Integer range 1 .. P8.Random (G8);         -- Won't trigger
   type R8 (D : M8 := 100) is record
      Name : String (1 .. D);
   end record;

end Oversize;
