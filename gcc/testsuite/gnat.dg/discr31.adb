-- { dg-do compile }
-- { dg-options "-O" }

package body Discr31 is

   function Log_Item(Packet : in Packet_Data_Type) return Log_Item_Type is
      None : Log_Item_Type(0);
   begin
      return None;
   end;

end Discr31;
