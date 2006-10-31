-- { dg-do run }

with Text_IO; use Text_IO;
with Self; use Self;
procedure Test_Self is
   It : Lim := G (5);
begin   
   Change (It, 10);
   if Get (It) /= 35 then 
      Put_Line ("self-referential aggregate incorrectly built");
   end if; 
end Test_Self;
