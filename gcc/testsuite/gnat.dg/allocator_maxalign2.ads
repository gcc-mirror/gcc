with System;

package Allocator_Maxalign2 is
   type Block is record
      X : Integer;
   end record;
   for Block'Alignment use Standard'Maximum_Alignment;

   Addr : System.Address;

   procedure Check;
end;
