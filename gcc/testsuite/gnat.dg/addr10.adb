--  { dg-do compile }

with System;

procedure Addr10 is
   type Limited_Type is limited record
      Element : Integer;
   end record;

   function Initial_State return Limited_Type is ((Element => 0));

   type Double_Limited_Type is
      record
         A : Limited_Type;
      end record;

   Double_Limited : Double_Limited_Type :=
      (A => Initial_State)
   with
      Volatile,
      Address => System'To_Address (16#1234_5678#);
begin
   null;
end Addr10;
