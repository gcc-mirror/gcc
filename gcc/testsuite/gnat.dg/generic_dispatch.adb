--  { dg-do run }

with generic_dispatch_p; use generic_dispatch_p;
procedure generic_dispatch is
   I : aliased Integer := 0;
   D : Iface'Class := Dispatching_Constructor (DT'Tag, I'access);
begin   
   null;   
end generic_dispatch;
