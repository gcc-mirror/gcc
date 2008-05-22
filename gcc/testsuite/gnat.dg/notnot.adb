--  { dg-do compile }
--  { dg-options "-gnatwr" }

procedure notnot (x, y : integer) is
begin
   if not (not (x = y)) then  -- { dg-warning "redundant double negation" }
      return;
   end if;
end;
