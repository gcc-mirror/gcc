package Thunk1_Pkg2 is

  type Root is tagged record
    I : Integer;
  end record;

  type Iface is interface;
  procedure Op (This : in out Iface; S : String) is abstract;

  type Ext is new Root and Iface with null record;

  procedure Op (This : in out Ext; S : String);

end Thunk1_Pkg2;
