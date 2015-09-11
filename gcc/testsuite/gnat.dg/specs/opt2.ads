-- { dg-do compile }
-- { dg-options "-O2" }

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces;            use Interfaces;

package Opt2 is

  type Arr is array (Unsigned_32 range <>) of Unbounded_String;

end Opt2;
