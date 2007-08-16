
with System;
package body addr2_p is
   procedure Process (Blk : Block) is
      use type System.Address;
   begin
      if Blk'Address /= B1'Address and then Blk'Address /= B2'Address then
         raise Program_Error;
      end if;
   end;
end;
