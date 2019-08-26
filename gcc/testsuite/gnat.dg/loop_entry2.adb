--  { dg-do compile }
--  { dg-options "-gnata" }

procedure Loop_Entry2 (S : String) is
   J : Integer := S'First;
begin
   while S(J..J+1) = S(J..J+1) loop
      pragma Loop_Invariant (for all K in J'Loop_Entry .. J => K <= J);
      J := J + 1;
   end loop;
end;
