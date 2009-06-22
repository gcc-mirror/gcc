-- { dg-do run }
-- { dg-options "-gnatp -O2" }

with Raise_Ce;

procedure Handle_And_Return is
begin
   begin
      Raise_CE;
      return;
   exception
      when others => null;
   end;

   begin
      Raise_CE;
      return;
   exception
      when others => null;
   end;
end;
