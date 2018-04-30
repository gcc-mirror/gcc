package Class_Wide3_Pkg is

   type Iface is interface;
   type Iface_Ptr is access all Iface'Class;

   procedure Put_Line (I : Iface'Class);

   type Root is tagged record
      I : Integer;
   end record;

   type Disc_Child (N : Integer) is new Root and Iface with record
      J : Integer;
   end record;

end Class_Wide3_Pkg;
