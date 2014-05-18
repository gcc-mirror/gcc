-- { dg-do run }

procedure Enum3 is
   type Enum is (Aaa, Bbb, Ccc);
   for Enum use (1,2,4);
begin
   for Lo in Enum loop
      for Hi in Enum loop
         declare
            subtype S is Enum range Lo .. Hi;
            type Vector is array (S) of Integer;
            Vec : Vector;
         begin
            for I in S loop
               Vec (I) := 0;
            end loop;
            if Vec /= (S => 0) then
               raise Program_Error;
            end if;
         end;
      end loop;
   end loop;
end;
