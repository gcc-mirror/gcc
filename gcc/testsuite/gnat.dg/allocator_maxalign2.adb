with System, System.Storage_Elements;
use System.Storage_Elements;

package body Allocator_Maxalign2 is

   Max_Align : constant Storage_Offset := Standard'Maximum_Alignment;

   procedure Validate is
      use type System.Address;
   begin
      if Addr mod Max_Align /= 0 then
         raise Program_Error;
      end if;
   end;

   procedure Check is
      I : Integer;
      B : Block;
      type Block_Access is access all Block;
      A : Block_Access;
   begin
      Addr := I'Address;
      Addr := B'Address;
      Validate;
      for I in 1 .. 50 loop
         A := new Block;
         Addr := A.all'Address;
         Validate;
      end loop;

   end;

end;
