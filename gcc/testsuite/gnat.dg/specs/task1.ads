-- { dg-do compile }
-- { dg-options "-gnatct" }

package Task1 is

   type Cable_Task_1 (C : Boolean) is limited private;

   type Cable_Rec is limited record
      Tsk_1 : Cable_Task_1 (C => False);
   end record;

private
   task type Cable_Task_1 (C : Boolean) is
end Cable_Task_1;

end Task1;
