with Inline16_Types; use Inline16_Types;

package body Inline16_Gen
with SPARK_Mode => On
is
   procedure Gfw_Image_Read (Data    : out Payload_Type)
      with SPARK_Mode => Off
   is
      Array_Len     : constant NvU32 := Data'Size / NvU8'Size;
      Array_Max_Idx : constant NvU32 := Array_Len - 1;
      type Payload_As_Arr_Type is new Arr_NvU8_Idx32(0 .. Array_Max_Idx);
      Data_As_Array : Payload_As_Arr_Type with Address => Data'Address;
   begin
      null;
   end Gfw_Image_Read;
end Inline16_Gen;
