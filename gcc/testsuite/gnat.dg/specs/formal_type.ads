-- { dg-do compile }

with Ada.Strings.Bounded;
package formal_type is
   generic 
      with package BI is
         new Ada.Strings.Bounded.Generic_Bounded_Length (<>);
      type NB is new BI.Bounded_String;
   package G is end; 
   package BI is new Ada.Strings.Bounded.Generic_Bounded_Length (30);
   type NB is new BI.Bounded_String;
   Thing : NB;
   package GI is new G (BI, NB);
end;    
