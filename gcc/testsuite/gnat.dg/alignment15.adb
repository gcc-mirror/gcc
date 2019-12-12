--  { dg-compile }

procedure Alignment15 is
   type T0 is record
      X : Integer;
   end record;
   for T0'Alignment use 0;

   type T00 is record
      X : Integer;
   end record with Alignment => 0;

   Dummy0  : T0;
   Dummy00 : T00;
begin
   null;
end;
