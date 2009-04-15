package sync1 is
   type Chopstick_Type is synchronized interface;
   
   type Chopstick is synchronized new Chopstick_Type with private;
private
   protected type Chopstick is new Chopstick_Type with
      entry Pick_Up;
      procedure Put_Down;
   private
      Busy : Boolean := False;
   end Chopstick;
end sync1;
