with overriding_ops_p; use overriding_ops_p;
package overriding_ops is
   task type Light_Programmer is new Device with
      overriding entry Set_Name (Name : Name_Type);
   end Light_Programmer;
   --  Object that represents a light 
   protected type Light is new Device with
      overriding procedure Set_Name (Name : Name_Type);
   private 
      L_Name : Name_Type;
   end Light;
end overriding_ops;
