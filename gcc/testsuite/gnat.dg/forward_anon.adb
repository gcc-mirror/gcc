--  { dg-do compile }

package body Forward_Anon is
   function Get_Current return access Object is
   begin
      return Current_Object;
   end;
end;
