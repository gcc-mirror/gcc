-- { dg-do compile }

with System;

package Size_Clause4 is

  type Rec is record
    A1 : System.Address;  
    A2 : System.Address;  
    I  : aliased Integer; 
  end record;
       
  for Rec use record
    A1 at 0  range 0 .. 63; 
    A2 at 8  range 0 .. 63;
    I  at 16 range 0 .. 31;  
  end record;
  for Rec'Size use 160;

end Size_Clause4;
