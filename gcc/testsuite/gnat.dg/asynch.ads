with Ada.Finalization;
package asynch is
   type t_ctrl is new Ada.Finalization.Controlled with record
      stuff : Natural := 0;
   end record;
   
   function null_ctrl return t_ctrl;
end asynch;
