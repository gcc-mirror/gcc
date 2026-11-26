-- { dg-options "-gnat2022" }

with Ada.Strings.Unbounded;

generic
package Aggr13_Pkg is
   type Arr is array (1 .. 3) of Ada.Strings.Unbounded.Unbounded_String;
   A : constant Arr :=
     [for I in Arr'Range => Ada.Strings.Unbounded.To_Unbounded_String ("x")];
end Aggr13_Pkg;
