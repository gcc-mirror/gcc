--  { dg-do run }
--  { dg-options "-gnato" }

procedure Overflow_Sum3 is

   function Ident (I : Integer) return Integer is
   begin
      return I;
   end;

   X : Short_Short_Integer := Short_Short_Integer (Ident (127));

begin
   if X+1 <= 127 then
      raise Program_Error;
   end if;
exception
   when Constraint_Error => null;
end;
