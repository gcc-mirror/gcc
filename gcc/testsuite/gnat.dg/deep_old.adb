--  { dg-options "-gnatws" }

procedure Deep_Old (X : Integer) is
begin
   begin
      if X = X'Old then
         null;
      end if;
   end;
end Deep_Old;
