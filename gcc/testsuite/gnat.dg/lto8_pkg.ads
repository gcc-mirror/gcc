with System;
with Unchecked_Conversion;

package Lto8_Pkg is

   type Task_Priority_T is new Natural;
   function Convert_To_System_Priority is
     new Unchecked_Conversion (Task_Priority_T, System.Priority);

   protected type Protected_Queue_T( PO_Priority : Task_Priority_T ) is
      pragma Priority (Convert_To_System_Priority (PO_Priority ));
      entry Seize;
   end Protected_Queue_T;

   Sema1 : protected_Queue_T (5);
   Sema2 : protected_Queue_T (10);

end Lto8_Pkg;
