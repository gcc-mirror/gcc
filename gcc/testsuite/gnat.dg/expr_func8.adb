--  { dg-do compile }
--  { dg-options "-gnata" }

procedure Expr_Func8 is

   type Node_Set is array (Positive range <>) of Integer;

   function Nodes return Node_Set is
     ((1,2,3,4,5,6,7,8,9));

   X1 : Boolean := (for all N of Nodes => N = N);

   function Predecessors (N : Integer) return Node_Set Is
      (Nodes (1 .. N - 1));
   function Successors (N : Integer) return Node_Set Is
      (Nodes (N + 1 .. Nodes'Last));

   pragma Assert
     (for all N of Nodes =>
       (for some S of Successors (N) => S = N));

   X2 : Boolean :=
     (for all N of Nodes =>
       (for some S of Successors (N) => S = N));

   X3 : Boolean :=
     (for all N of Nodes =>
       (for some S of Successors (N) => S = N)) with Ghost;

   pragma Assert
      (for all N of Nodes =>
      (for all P of Predecessors (N) =>
      (for some S of Successors (P) => S = N)));

begin
   null;
end;
