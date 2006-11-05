-- { dg-do compile }

with Layered_Abstraction_P;
with layered_abstraction;
procedure layered_instance is
   package s1 is new Layered_Abstraction_P (Integer, 15);
   package S2 is new Layered_Abstraction_P (Integer, 20);
   package Inst is new layered_abstraction (S1, S2);
begin   
   null;   
end;    
