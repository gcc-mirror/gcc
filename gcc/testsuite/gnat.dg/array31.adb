--  { dg-do run }

procedure Array31 is

   type Boolean_Access is access Boolean;

   type Boolean_Access_Array is
     array (Positive range <>) of not null Boolean_Access;

   X : constant Boolean_Access_Array := (1 => new Boolean'(False));
   Y : constant Boolean_Access_Array := X & X;

begin
   null;
end;
