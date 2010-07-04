-- { dg-do run }
-- { dg-options "-flto" { target lto } }

with Lto7_Pkg; use Lto7_Pkg;

procedure Lto7 is
   view2 : access Iface_2'Class;
   obj   : aliased DT := (m_name => "Abdu");
begin
   view2 := Iface_2'Class(obj)'Access;
   view2.all.op2;
end;
