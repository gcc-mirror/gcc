-- { dg-do compile }

package body Bit_Packed_Array4  is

   procedure Process (M : Message_Type) is
      D : Data_Type;
   begin
      D := M.Data;
   end;

end Bit_Packed_Array4;
