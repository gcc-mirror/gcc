package Interface8 is
   type Iface is interface;

   type Root is abstract tagged null record;

   type Child is new Root and Iface with record
      Interface_1 : access Iface'Class;
   end record;

   function Get_Iface (This : Child) return not null access Iface'Class;
end;
