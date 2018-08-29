--  { dg-do compile }

package body Remote_Call_Iface is
   procedure Proc is begin null; end;
begin
   Proc;
end Remote_Call_Iface;
