-- { dg-do compile { target *-*-linux* } }
-- { dg-options "-gdwarf-2" }

procedure Blocklocs (Choice : Integer; N : in out Integer) is
begin
   if Choice > 0 then
      declare -- line 7
         S : String (1 .. N * 2);
         pragma Volatile (S);
      begin
	 S := (others => 'B');
      end;    -- line 12
   else
      declare -- line 14
	 S : String (1 .. N );
	 pragma Volatile (S);
      begin
	 S := (others => '1');
      end;    -- line 19
   end if;
end;
   
-- { dg-final { scan-assembler "loc 1 7" } }
-- { dg-final { scan-assembler "loc 1 12" } }
-- { dg-final { scan-assembler "loc 1 14" } }
-- { dg-final { scan-assembler "loc 1 19" } }
