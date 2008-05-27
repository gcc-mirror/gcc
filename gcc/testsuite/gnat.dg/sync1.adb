--  { dg-do compile }
package body sync1 is
   protected body Chopstick is

     entry Pick_Up when not Busy is
     begin
        Busy := True;
     end Pick_Up;

     procedure Put_Down is
     begin
        Busy := False;
     end Put_Down;
   end Chopstick;
end sync1;
