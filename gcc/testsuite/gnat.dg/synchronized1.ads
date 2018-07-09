package Synchronized1
  with SPARK_Mode,
       Abstract_State => (State with Synchronous),
       Initializes    => State
is
   procedure Force_Body;
end Synchronized1;
