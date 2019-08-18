--  { dg-do compile }
--  { dg-options "-gnatwc" }

procedure Loop_Entry1 (X, Y : in out Integer) is
begin
   while X < Y loop
      pragma Loop_Invariant
        (X >= X'Loop_Entry and then Y <= Y'Loop_Entry);

      X := X + 1;
      Y := Y - 1;
   end loop;
end Loop_Entry1;
