--  { dg-do compile }

package body asynch is
   function null_ctrl return t_ctrl is
   begin
      return (Ada.Finalization.Controlled with stuff => 0);
   end null_ctrl;
   
   procedure Proc (msg : String; c : t_ctrl := null_ctrl) is
   begin
      null;
   end Proc;
   
   task type tsk;
   task body tsk is
   begin
      select                                                    
         delay 10.0;                                            
         Proc ("A message.");
      then abort
         null;
      end select;
   end tsk;
end asynch;
