with GNAT.Heap_Sort_G;
function sort1 (S : String) return String is
   Result : String (1 .. S'Length) := S;
   Temp : Character;

   procedure Move (From : Natural; To : Natural) is 
   begin
      if From = 0 then Result (To) := Temp;
      elsif To = 0 then Temp := Result (From);
      else Result (To) := Result (From);                       
              end if;                                          
   end Move; 
   
   function Lt (Op1, Op2 : Natural) return Boolean is
   begin
      if Op1 = 0 then return Temp < Result (Op2);
      elsif Op2 = 0 then return Result (Op1) < Temp;
      else return Result (Op1) < Result (Op2);
      end if;
   end Lt;
   
   package SP is new GNAT.Heap_Sort_G (Move, Lt);
   
begin
   SP.Sort (S'Length);
   return Result;
end;
