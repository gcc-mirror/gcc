--  { dg-do run }

with Task2_Pkg; use Task2_Pkg;

procedure Task2 is
   X : access T2'Class := new T2;
begin
   null;
end Task2;
