-- { dg-do compile }
-- { dg-options "-gnatws" }

package body Synchronized1
  with SPARK_Mode,
       Refined_State => (State => Curr_State)
is
   type Reactor_State is (Stopped, Working) with Atomic;

   Curr_State : Reactor_State
     with Async_Readers, Async_Writers;

   procedure Force_Body is null;
end Synchronized1;
