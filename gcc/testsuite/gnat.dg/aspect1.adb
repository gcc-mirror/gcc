--  { dg-do compile }

with Aspect1_Horizontal;
with Aspect1_Vectors_2D;

procedure Aspect1 is
   type Speed is new Float;
   package Distances is new Aspect1_Vectors_2D (Float);
   package Velocities is new Aspect1_Vectors_2D (Speed);
   package Motion is new Aspect1_Horizontal (Distances, Velocities);
begin
   null;
end;
