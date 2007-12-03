package body Bug_Elaboration_Code is

   procedure Increment_I is
   begin
      I := I + 1;
   end Increment_I;

begin
   I := 5;
   Increment_I;
   J := I;
end Bug_Elaboration_Code;
