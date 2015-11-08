-- { dg-do run }

with System;

procedure SSO7 is

   type Short_Int is mod 2**16;

   type Rec1 is record
      F1 : Short_Int;
      F2 : Short_Int;
   end record;
   for Rec1 use record
      F1 at 0 range  0 .. 15;
      F2 at 0 range 16 .. 31;
   end record;
   for Rec1'Bit_Order use System.High_Order_First;
   for Rec1'Scalar_Storage_Order use System.High_Order_First;

   type Rec2 is record
      R1 : Rec1;
   end record;
   for Rec2 use record
      R1 at 0 range 0 .. 31;
   end record;
   for Rec2'Bit_Order use System.High_Order_First;
   for Rec2'Scalar_Storage_Order use System.High_Order_First;

   type Rec3 is record
      Data : Rec1;
   end record;
   for Rec3 use record
      Data at 0 range 1 .. 32;
   end record;
   for Rec3'Bit_Order use System.High_Order_First;
   for Rec3'Scalar_Storage_Order use System.High_Order_First;

   procedure Copy (Message : in Rec3) is
      Local : Rec2;
   begin
      Local := (R1 => Message.Data);
      if Local.R1 /= Message.Data then
         raise Program_Error;
      end if;
   end;

   Message : Rec3;

begin
   Message := (Data => (2, 3));
   Copy(Message);
end;
