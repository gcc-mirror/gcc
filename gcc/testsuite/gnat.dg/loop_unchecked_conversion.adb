-- { dg-do compile }
-- { dg-options "-gnatws -O" }

with Unchecked_Conversion;

package body loop_unchecked_conversion is

   type Byte is mod 2**8;

   type List is array (Natural range <>) of Byte;

   subtype Integer_List is List (1 .. 4);

   function Integer_Down is new
     Unchecked_Conversion (Source => Integer, Target => Integer_List);

   type Storage (Size : Integer) is
      record
         Data : List (1 .. Size);
      end record;

   type Storage_Pointer is access Storage;

   The_Data_Storage : Storage_Pointer;

   procedure slice is
   begin
      for I in 0 .. 1 loop
         The_Data_Storage.Data (I+1 .. I+4) := Integer_Down (I);
      end loop;
   end;

end loop_unchecked_conversion;
