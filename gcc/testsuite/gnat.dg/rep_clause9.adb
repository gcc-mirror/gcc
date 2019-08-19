--  { dg-do run }

procedure Rep_Clause9 is

   type Day_Of_Week
      is (Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday);

   type New_Day_Of_Week is new Day_Of_Week  range Monday .. Friday;
   for New_Day_Of_Week use
      (Sunday => -4, Monday => -2, Tuesday => 1, Wednesday => 100,
       Thursday => 1000, Friday => 10000, Saturday => 10001);

   V1 : New_Day_Of_Week;

begin
   if Integer'Image(New_Day_Of_Week'Pos(Monday)) /= " 1" then
      raise Program_Error;
   end if;
   V1 := Monday;
   if Integer'Image(New_Day_Of_Week'Pos(V1)) /= " 1" then
      raise Program_Error;
   end if;
end;
