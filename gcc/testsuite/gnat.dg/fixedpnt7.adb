--  { dg-do run }

procedure Fixedpnt7 is
   type F1 is delta 1.0 range -2.0**63 .. 0.0
     with Small => 1.0;
   type F2 is delta 4.0 range 0.0 .. 2.0**64
     with Small => 4.0;
   type D is delta 1.0 digits 18;

   XX : constant := -2.0**63;
   YY : constant := 2.0**64;

   X : F1 := XX;
   Y : F2 := YY;
   U : D := D'Round(X / Y);
begin
   if U /= -1.0 then
      raise Program_Error;
   end if;
end Fixedpnt7;