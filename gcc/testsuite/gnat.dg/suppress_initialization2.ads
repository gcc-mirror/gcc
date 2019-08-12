pragma Initialize_Scalars;

with System;

package Suppress_Initialization2 is

   subtype Sub_Addr is System.Address with Suppress_Initialization;

   O : Sub_Addr with Thread_Local_Storage;  -- OK: no error should be reported

   procedure Dummy;

end Suppress_Initialization2;
