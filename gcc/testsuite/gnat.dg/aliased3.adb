-- { dg-do compile }

procedure Aliased3 is

   function F (R : aliased Integer) return access constant Integer is
   (R'Access);

   X : access constant Integer;

begin
   declare
      R : aliased Integer := 123;
      Y : access constant Integer;
   begin
      Y := F (R); -- { dg-bogus "wrong accessibility in assignment" }
      X := F (R); -- { dg-error "wrong accessibility in assignment" }
   end;
end;
