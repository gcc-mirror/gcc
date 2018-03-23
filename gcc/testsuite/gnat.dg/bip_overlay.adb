--  { dg-do compile }

with System;

package body BIP_Overlay
with
   SPARK_Mode
is
   function Init return X
   is
   begin
      return Result : X do
         Result.E := 0;
      end return;
   end Init;

   I : X := Init
   with
      Volatile,
      Async_Readers,
      Address => System'To_Address (16#1234_5678#);

end BIP_Overlay;
