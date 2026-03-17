-- PR ada/124224
-- { dg-do compile }
-- { dg-options "-gnat2022" }

with Ada.Containers.Ordered_Maps;

package Aggr12 is

  package Maps is new
    Ada.Containers.Ordered_Maps (Character, Boolean);

  M : Maps.Map := [for C in Character => False];

end Aggr12;
