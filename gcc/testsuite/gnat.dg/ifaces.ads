
package Ifaces is
   type Iface_1 is interface;
   procedure op1(this : Iface_1) is abstract;
-- 
   type Iface_2 is interface;
   procedure op2 (this : Iface_2) is abstract;
--    
   type Root is new Iface_1 with record
      m_name : String(1..4);
   end record;
-- 
   procedure op1 (this : Root);
--       
   type DT is new Root and Iface_2 with null record;
   procedure op2 (this : DT);
end;
