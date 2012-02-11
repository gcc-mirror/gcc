-- { dg-do compile }

with Aggr4_Pkg; use Aggr4_Pkg;

package Aggr4 is

   C : constant Rec3 := (Data => (D => One, Value => Zero));

end Aggr4;
