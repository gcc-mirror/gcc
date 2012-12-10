package Vect9_Pkg is

   type Unit is array (1 .. 4) of Float;
   for Unit'Alignment use 32;
   pragma Machine_Attribute (Unit, "vector_type");
   pragma Machine_Attribute (Unit, "may_alias");

   Zero_Unit : constant Unit := (others => 0.0);

   function Mul (X : in Unit; Y : in Unit) return Unit;
   function "+"(Left, Right : Unit) return Unit;
   function "*"(Left, Right : Unit) return Unit;

   type Unit_Vector is array (Positive range <>) of Unit;
   type Vector_Access is access all Unit_Vector;

end Vect9_Pkg;
