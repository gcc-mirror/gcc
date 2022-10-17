-- { dg-do compile}
-- { dg-options "-gnatwa" }

with Ada.Text_IO;
use Ada.Text_IO;
package body renaming1 is
   procedure Fo (A : Ada.Text_IO.File_Access) is
   begin
      if A = Ada.Text_IO.Standard_Output then
         null;
      end if;
   end Fo;
end;
