--  { dg-do compile }

procedure Discr50 is
   type My_Record (D : Integer) is record
      A : Integer;
   end record;

   B : My_Record (Positive range 1 .. 10);  -- { dg-error "a subtype indication is not a valid discriminant constraint" }
begin
   null;
end Discr50;
