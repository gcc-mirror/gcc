with System;

package Thin_Pointer1 is

   type Stream is array (Integer range <>) of Character;

   type Stream_Ptr is access Stream;
   for Stream_Ptr'Size use Standard'Address_Size;

   type Buf is record
      A : System.Address;
   end record;

   type Buf_Wrapper is record
      B : Buf;
   end record;

   type Buf_Ptr is access Buf_Wrapper;

   procedure Set_Buffer (AD : Buf_Ptr; Buffer : Stream_ptr);

end Thin_Pointer1;
