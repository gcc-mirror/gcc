
-- { dg-do compile }
-- { dg-options "-O2 -gnatp" }

function Scalar_Mode_Agg_Compare_Loop return Boolean is
   S : constant String (1 .. 4) := "ABCD";
   F : constant Natural := S'First;
   L : constant Natural := S'Last;
begin
   for J in F .. L - 1 loop
      if S (F .. F) = "X" or (J <= L - 2 and S (J .. J + 1) = "YY") then
         return True;
      end if;
   end loop;

   return False;
end;

