--  { dg-do compile }
--  { dg-options "-gnatN" }

with Inline16_Types; use Inline16_Types;
with Inline16_Gen;

procedure Inline16 is
   type TYPE1 is record
      f1 : NvU32;
      f2 : NvU32;
      f3 : NvU32;
   end record
      with Size => 96, Object_Size => 96;

   package Gfw_Image_Read_Pkg1 is new Inline16_Gen (Payload_Type => TYPE1);
   use Gfw_Image_Read_Pkg1;
   procedure Get_Boot_Block_Info(Status : out Integer)
   is
      Ifr_Fixed_Min       : TYPE1;
   begin
      Gfw_Image_Read(Ifr_Fixed_Min);
      Status := 13;
   end Get_Boot_Block_Info;
begin
   null;
end Inline16;
