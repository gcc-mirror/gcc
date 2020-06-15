with Ada.Finalization;
with Interfaces;
with System;

package Opt85 is

   type Data_Type is record
      Value : Interfaces.Integer_16;
   end record;
   for Data_Type use record
      Value at 0 range 0 .. 15;
   end record;
   for Data_Type'Alignment use 1;
   for Data_Type'Size use 2 * System.Storage_Unit;
   for Data_Type'Bit_Order use System.High_Order_First;
   for Data_Type'Scalar_Storage_Order use System.High_Order_First;

   type Header_Type is array (1 .. 1) of Boolean;

   type Record_Type is new Ada.Finalization.Controlled with record
      Header : Header_Type;
      Data   : Data_Type;
   end record;

   function Create (Value : Integer) return Record_Type;

end Opt85;
