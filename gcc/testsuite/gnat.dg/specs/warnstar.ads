--  { dg-do compile }

pragma Warnings (Off, "*bits of*unused");
package warnstar is
   type r is record
      a : integer;
   end record;
   
   for r use record
      a at 0 range 0 .. 1023;
   end record;
end warnstar;
