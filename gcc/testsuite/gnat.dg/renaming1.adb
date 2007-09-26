-- { dg-do compile}
-- { dg-options "-gnatwa" }

with Text_IO;
use Text_IO;
package body renaming1 is
   procedure Fo (A : Text_IO.File_Access) is
   begin
      if A = Text_IO.Standard_Output then
         null;
      end if;
   end Fo;
end;
