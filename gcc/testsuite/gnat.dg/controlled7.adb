-- PR ada/53766
-- Reported by Duncan Sands <baldrick@gcc.gnu.org>

-- { dg-do compile }
-- { dg-options "-gnatp" }

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Controlled7 is

   procedure Proc (Offset : Storage_Offset) is
   begin
      if Offset + Unbounded_String'Max_Size_In_Storage_Elements >= 16 then
         raise Program_Error;
      end if;
   end;

end Controlled7;
