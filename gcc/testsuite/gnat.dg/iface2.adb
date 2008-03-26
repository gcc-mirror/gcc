--  { dg-do compile }
package body Iface2 is
   procedure change (This, That : Prot.Any_Future) is
   begin
      null;
   end;
end Iface2;
