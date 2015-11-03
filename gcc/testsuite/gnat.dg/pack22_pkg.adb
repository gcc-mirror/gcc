package body Pack22_Pkg is

   package body Bit_Map_Generic is

      function "xor" (L, R : List) return List is
         Temp : List;
         for Temp'address use Temp_buffer'address;
      begin
         Temp.Bits := L.Bits xor R.Bits;
         Temp.Counter.Counter := 0;
         return Temp;
      end;

   end Bit_Map_Generic;

end Pack22_Pkg;
