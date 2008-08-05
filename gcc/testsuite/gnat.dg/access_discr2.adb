--  { dg-do run }

procedure access_discr2 is
   type X (I : not null access Integer) is tagged null record;
   
   I : aliased Integer := 8;
   Y : X (I'Access);
begin
   null;
end access_discr2;
