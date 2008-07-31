package Sync_Iface_Test is
   type Iface is limited interface;
   function First (Obj : Iface) return Natural is abstract;

   protected type Buffer is new Iface with
      procedure Dummy;
   end;
   overriding function First (Obj : Buffer) return Natural;

   procedure Do_Test (Dummy : Natural; Item  : Buffer);
end;
