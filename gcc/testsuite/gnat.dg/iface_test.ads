package Iface_Test is
   type Iface_1 is interface;
   type Iface_2 is interface;
   
   procedure Prepare_Select
     (DB   : Iface_1;
      Iter : in out Iface_2'Class) is abstract; 
        
   type DT_1 is new Iface_1 with null record;
        
   type Iterator is new Iface_2 with record
      More : Boolean;
   end record;

   overriding procedure Prepare_Select
     (DB   : DT_1;
      Iter : in out Standard.Iface_Test.Iface_2'Class);
end;
