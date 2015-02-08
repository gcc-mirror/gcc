-- { dg-do compile }

package body Addr8 is

   procedure Proc (B: Bytes) is
      O: Integer;
      for O'Address use B'Address;
   begin
      null;
   end;

end Addr8;
