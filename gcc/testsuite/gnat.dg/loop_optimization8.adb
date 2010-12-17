-- { dg-do run }
-- { dg-options "-O -gnatn" }

with Loop_Optimization8_Pkg1;

procedure Loop_Optimization8 is

  Data : Loop_Optimization8_Pkg1.T;

  procedure Check_1 (N : in Natural) is
  begin
     if N /= 0 then
       for I in 1 .. Data.Last loop
         declare
           F : constant Natural := Data.Elements (I);
         begin
           if F = N then
              raise Program_Error;
           end if;
         end;
       end loop;
     end if;
  end;

  procedure Check is new Loop_Optimization8_Pkg1.Iter (Check_1);

begin
  Data := Loop_Optimization8_Pkg1.Empty;
  Check;
end;
