--  { dg-do run }

with Ifaces; use Ifaces;
procedure test_ifaces is
   view2 : access Iface_2'Class;
   obj   : aliased DT := (m_name => "Abdu");
begin
   view2 := Iface_2'Class(obj)'Access;
   view2.all.op2;
end;
