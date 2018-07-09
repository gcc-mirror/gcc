package Sync2 with
  SPARK_Mode,
  Abstract_State => (State with Synchronous)
is
   pragma Elaborate_Body;
end Sync2;
