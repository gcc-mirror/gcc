-- { dg-do run }

procedure Membership_Real1 is

   type Fix is delta 0.25 range -8.0 .. 8.0;

   X : constant := 0.1;
   Y : constant := 0.3;

   F : Float := X;
   G : Fix := 0.25;

begin
   if not (X in F .. 1.0) then
      raise Program_Error;
   end if;

   if not (0.1 in F .. 1.0) then
      raise Program_Error;
   end if;

   if X not in F .. 1.0 then
      raise Program_Error;
   end if;

   if not (Y in -1.0 .. G) then
      raise Program_Error;
   end if;
end;
