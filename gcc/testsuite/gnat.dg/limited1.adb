--  { dg-do run }

with Limited1_Outer; use Limited1_Outer;

procedure Limited1 is
   X : Outer_Type := Make_Outer;
begin
   null;
end;
