--  { dg-do run }

with GNAT.Time_Stamp;
use  GNAT.Time_Stamp;

procedure test_time_stamp is
   S : constant String := Current_Time;
   
   function NN (S : String) return Boolean is
   begin
      for J in S'Range loop
         if S (J) not in '0' .. '9' then
            return True;
         end if;
      end loop;
      return False;
   end NN;

begin
   if S'Length /= 22
     or else S (5) /= '-'
     or else S (8) /= '-'
     or else S (11) /= ' '
     or else S (14) /= ':'
     or else S (17) /= ':'
     or else S (20) /= '.'
     or else NN (S (1 .. 4))
     or else NN (S (6 .. 7))
     or else NN (S (9 .. 10))
     or else NN (S (12 .. 13))
     or else NN (S (15 .. 16))
     or else NN (S (18 .. 19))
     or else NN (S (21 .. 22))
   then
      raise Program_Error;
   end if;
end;
