-- { dg-do compile }

procedure Warn7 is

   procedure Nested;
   pragma No_Return (Nested);

   procedure Nested is
   begin
      raise Constraint_Error;
   exception
      when Constraint_Error =>
         raise;
   end;

begin
   Nested;
end;
