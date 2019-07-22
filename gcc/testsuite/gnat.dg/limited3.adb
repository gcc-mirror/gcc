--  { dg-do run }

with Limited3_Pkg; use Limited3_Pkg;

procedure Limited3 is
   R1 : Rec := F (15);
   R2 : Rec := F (-1);
   R3 : Var_Rec := FS (20);
begin
   null;
end Limited3;
