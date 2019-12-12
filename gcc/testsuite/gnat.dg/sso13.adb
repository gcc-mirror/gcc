--  { dg-do compile }

with System;

procedure SSO13 is
   type Pulse_Buffer_Type is abstract tagged null record
    with Bit_Order => System.High_Order_First,
         Scalar_Storage_order =>System.High_order_First;  --  { dg-warning "scalar storage order specified but no component clause" }
   type Pulse_Train_Type is abstract new Pulse_Buffer_Type with null record;
   type WO_Pulse_Train_Type is new Pulse_Train_Type with null record;
   type WO_Confirmation_Pulse_Train_Type is new WO_Pulse_Train_Type with record
      null;
   end record;

   type Update_Dwell_Type is abstract tagged null record
     with Bit_Order => System.High_Order_First,
          Scalar_Storage_order =>System.High_order_First;  --  { dg-warning "scalar storage order specified but no component clause" }
   type Confirmation_Dwell_Type is new Update_Dwell_Type with
   record
      Pulses : aliased WO_Pulse_Train_Type; -- (Location of Error #1)
   end record;
begin
   null;
end;
