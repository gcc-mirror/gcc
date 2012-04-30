-- { dg-do compile }
-- { dg-options "-O2" }

with Unchecked_Conversion;
with System;

package body Warn6 is

  function Conv is new Unchecked_Conversion (System.Address, Q_T);

  procedure Dummy is begin null; end;

end Warn6;
