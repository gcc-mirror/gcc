-- { dg-do compile }

package body Addr7 is

   procedure Proc (B: aliased Bytes) is
      O: Integer;
      for O'Address use B'Address;
   begin
      null;
   end;

end Addr7;
