package overriding_ops_p is
   subtype Name_Type is String (1 .. 30); 
   type Device is synchronized interface;
   --  Base type of devices 
   procedure Set_Name (Object : in out Device; Name : Name_Type)
     is abstract;
   --  Set the name of the Device
end overriding_ops_p;
