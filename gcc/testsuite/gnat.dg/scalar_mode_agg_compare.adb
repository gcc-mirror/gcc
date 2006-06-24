-- { dg-do run }

procedure Scalar_Mode_Agg_Compare is

   type Point is record
      Mapped : Boolean;
      Tag    : String (1 .. 2);  -- HImode
   end record;
   pragma Pack (Point);          -- Tag possibly at bitpos 1

   function My_Point return Point is
   begin
      return (Mapped => True, Tag => "XX");
   end;

   A, B : Point := My_Point;
begin
   -- The comparison below should find the two Tag fields equal and not
   -- attempt to take their address, which might not be byte aligned.

   if A.Tag /= B.Tag then
      raise Program_Error;
   end if;
end;

