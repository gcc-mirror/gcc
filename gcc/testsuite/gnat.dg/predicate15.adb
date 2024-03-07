-- { dg-do compile }
-- { dg-options "-gnata" }

procedure Predicate15 is

   type Grid is array (Positive range <>) of Integer with
      Dynamic_Predicate => Grid'First = 1;

   type Grid_Ptr is access Grid;

   Data : Grid_Ptr := new Grid (1 .. 10);

begin
   null;
end;
