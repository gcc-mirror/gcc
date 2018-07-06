--  { dg-do compile }

package body Sync2 with
  SPARK_Mode,
  Refined_State => (State => (A, P, T))
is
   A : Integer := 0 with Atomic, Async_Readers, Async_Writers;

   protected type Prot_Typ is
   private
      Comp : Natural := 0;
   end Prot_Typ;

   P : Prot_Typ;

   task type Task_Typ;

   T : Task_Typ;

   protected body Prot_Typ is
   end Prot_Typ;

   task body Task_Typ is
   begin
      null;
   end Task_Typ;
end Sync2;
