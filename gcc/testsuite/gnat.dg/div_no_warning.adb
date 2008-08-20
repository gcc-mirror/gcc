--  { dg-do compile }

procedure div_no_warning is
   Flag : constant Boolean := False;
   Var : Boolean := True; 
   function F return Boolean is
   begin
      return Var;
   end F;
   Int : Integer := 0;
begin
   if Flag and then F then
      Int := Int / 0;
   end if;
end div_no_warning;
