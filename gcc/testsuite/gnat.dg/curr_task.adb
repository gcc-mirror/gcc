-- { dg-do run }
-- { dg-options "-gnatws" }

with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Task_Identification;

procedure Curr_Task is

   use Ada.Task_Identification;

   --  Simple semaphore

   protected Semaphore is
      entry Lock;
      procedure Unlock;
   private
      TID        : Task_Id := Null_Task_Id;
      Lock_Count : Natural := 0;
   end Semaphore;

   ----------
   -- Lock --
   ----------

   procedure Lock is
   begin
      Semaphore.Lock;
   end Lock;

   ---------------
   -- Semaphore --
   ---------------

   protected body Semaphore is

      ----------
      -- Lock --
      ----------

      entry Lock when Lock_Count = 0
        or else TID = Current_Task
      is
      begin
         if not
           (Lock_Count = 0
            or else TID = Lock'Caller)
         then
            Ada.Text_IO.Put_Line
              ("Barrier leaks " & Lock_Count'Img
                 & ' ' & Image (TID)
                 & ' ' & Image (Lock'Caller));
         end if;

         Lock_Count := Lock_Count + 1;
         TID := Lock'Caller;
      end Lock;

      ------------
      -- Unlock --
      ------------

      procedure Unlock is
      begin
         if TID = Current_Task then
            Lock_Count := Lock_Count - 1;
         else
            raise Tasking_Error;
         end if;
      end Unlock;

   end Semaphore;

   ------------
   -- Unlock --
   ------------

   procedure Unlock is
   begin
      Semaphore.Unlock;
   end Unlock;

   task type Secondary is
      entry Start;
   end Secondary;

   procedure Parse (P1 : Positive);

   -----------
   -- Parse --
   -----------

   procedure Parse (P1 : Positive) is
   begin
      Lock;
      delay 0.01;

      if P1 mod 2 = 0 then
         Lock;
         delay 0.01;
         Unlock;
      end if;

      Unlock;
   end Parse;

   ---------------
   -- Secondary --
   ---------------

   task body Secondary is
   begin
      accept Start;

      for K in 1 .. 20 loop
         Parse (K);
      end loop;

      raise Constraint_Error;

   exception
      when Program_Error =>
         null;
   end Secondary;

   TS : array (1 .. 2) of Secondary;

begin
   Parse (1);

   for J in TS'Range loop
      TS (J).Start;
   end loop;
end Curr_Task;
