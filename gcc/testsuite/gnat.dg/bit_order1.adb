--  { dg-do compile }

with System;

procedure Bit_Order1 is

   type Sample_Ttype is tagged record
      Data : Natural;
   end record;

   type Other_Type is new Sample_Ttype with record
      Other_Data : String (1 .. 100);
   end record;

   for Other_Type'Bit_Order use System.High_Order_First; --  { dg-error "Bit_Order cannot be defined for record extensions" }
begin
   null;
end;
