--  { dg-do compile }

package body Interface8 is
   function Get_Iface (This : Child) return not null access Iface'Class
   is
   begin
      return This.Interface_1;
   end;
end;
