-- { dg-do compile }

with System;

package body Bit_Packed_Array5 is

   function Inv (Word : Word_Type) return Word_Type is
      W : Word_Type := Word;
      pragma Volatile (W);

      A_W : constant System.Address := W'Address;

      V : Short_Bit_Array_Type;
      for V'Address use A_W;
      pragma Volatile (V);
   begin
      for I in V'Range loop
          V (I) := not V (I);
      end loop;
      return W;
   end;

end Bit_Packed_Array5;
