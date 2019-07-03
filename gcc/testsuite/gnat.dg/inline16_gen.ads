generic
   type Payload_Type is private;
package Inline16_Gen
with SPARK_Mode => On
is
   procedure Gfw_Image_Read(Data   : out Payload_Type)
     with Inline_Always;

end Inline16_Gen;
