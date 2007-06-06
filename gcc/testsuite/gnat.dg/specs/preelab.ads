-- { dg-do compile }

with Ada.Finalization;
package preelab is
   type T is limited private;
   pragma Preelaborable_Initialization (T);
private    
   type T is new Ada.Finalization.Limited_Controlled with null record;
end preelab;
