--  { dg-do compile }

procedure Fixedpnt4 is
   type T is delta 2.0/5.0 range -10.0 .. 10.0 with Small => 2.0/5.0;
   type T2 is delta 1.0/25.0 range -10.0 .. 10.0 with Small => 1.0/25.0;

   X : T := 1.0;
   Y : T2;
begin
   Y := X / X;
   Y := X / (X + X);
   Y := X / (X + X + 1.0);
   Y := (X + X) * (X + X);
end;
