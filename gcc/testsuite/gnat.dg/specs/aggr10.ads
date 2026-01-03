-- PR ada/123371
-- { dg-do compile }
-- { dg-options "-gnat2022" }

with Ada.Containers.Ordered_Maps;

package Aggr10 is

  package Maps is new Ada.Containers.Ordered_Maps (Integer, Integer);

  function F return Maps.Map is ([]);

  A : Maps.Map := [for I of Maps.Map'[] => 1]; --  Works
  B : Maps.Map := [for I of A => 1]; --  Works
  C : Maps.Map := [for I of B use 1 => 1]; --  Works
  D : Maps.Map := [for I of F => 1]; --  Works

  X : Maps.Map := [for I of Maps.Map'[] use 1 => 1]; --  Infinite loop
  Y : Maps.Map := [for I of F use 1 => 1]; --  Infinite loop

end Aggr10;
