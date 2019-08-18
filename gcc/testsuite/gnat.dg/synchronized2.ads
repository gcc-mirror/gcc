--  { dg-do compile }
package Synchronized2 with SPARK_Mode, Abstract_State => (State with Synchronous) is
   procedure Dummy;
end;
