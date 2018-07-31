--  { dg-do compile }

with Part_Of1.Private_Generic;

package body Part_Of1.Instantiation
with
   Refined_State => (State => Inst.State)
is
   package Inst is new Part_Of1.Private_Generic;
end Part_Of1.Instantiation;
