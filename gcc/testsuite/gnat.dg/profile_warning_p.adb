package body profile_warning_p is
   procedure Proc is begin null; end Proc;
   
   task type T is
   end T;
   
   task body T is
   begin
      null;
   end;
   
   type A_T is access T;
   
   procedure Do_Stuff is
      P : A_T;
   begin
      P := new T;
   end Do_Stuff;

end;
