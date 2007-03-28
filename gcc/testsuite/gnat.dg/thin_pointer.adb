-- { dg-do compile }
-- { dg-options "-O" }

package body Thin_Pointer is

   procedure Set_Buffer (AD : Buf_Ptr; Buffer : Stream_ptr) is
   begin
      AD.B.A := Buffer (Buffer'First)'Address;
   end Set_Buffer;

end Thin_Pointer;
