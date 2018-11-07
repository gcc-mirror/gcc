with Addr12_B;
with Addr12_C;
with System;

package body Addr12_A is

   First_Address  : constant System.Address := Addr12_C.First'Address;
   Second_Address : constant System.Address := Addr12_C.Second'Address;

   First_Channel : Addr12_B.Shared_Context_Type := Addr12_B.Initial_State
   with Volatile, Async_Readers, Address => First_Address;

   Second_Channel : Addr12_B.Shared_Context_Type := Addr12_B.Initial_State
   with Volatile, Async_Readers;

   for Second_Channel'Address use Second_Address;

   procedure Do_Stuff is null;

end Addr12_A;
