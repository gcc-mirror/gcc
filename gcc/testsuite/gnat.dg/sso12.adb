-- { dg-do compile }

with Ada.Unchecked_Deallocation;
with System;

procedure SSO12 is
   type Rec is abstract tagged null record;
   for Rec'Scalar_Storage_Order use System.High_Order_First;  --  { dg-warning "scalar storage order specified but no component clause" }
   for Rec'Bit_Order use System.High_Order_First;

   type Rec_Acc is access all Rec'Class;

   procedure Free is new Ada.Unchecked_Deallocation (Rec'Class, Rec_Acc);
   X : Rec_Acc;
begin
   Free (X);
end SSO12;
