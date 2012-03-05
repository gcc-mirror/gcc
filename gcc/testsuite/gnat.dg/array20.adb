-- { dg-do assemble }

package body Array20 is

   type Arr is array (Positive range <>) of Integer;

   type P_Arr is access Arr;

   N : constant P_Arr := null;

   Table : P_Arr := N;

end Array20;
