with Ada.Finalization;
package task_name is
   type Base_Controller is
     abstract new Ada.Finalization.Limited_Controlled with null record;

   type Extended_Controller is
     abstract new Base_Controller with private;

   type Task_Object (Controller : access Extended_Controller'Class) is
     limited private;
private
   type String_Access is access string;

   type Extended_Controller is
     abstract new Base_Controller with record
        Thread : aliased Task_Object (Extended_Controller'Access);
        Name   : String_Access := new string'("the_name_of_the_task");
     end record;

   task type Task_Object (Controller : access Extended_Controller'Class) is           pragma Task_Name (Controller.Name.all);
   end Task_Object;
end;
