-- { dg-do compile }

package body overriding_ops is
   task body Light_Programmer is
   begin
      accept Set_Name (Name : Name_Type);
   end Light_Programmer;

   protected body Light is
      procedure Set_Name (Name : Name_Type) is
      begin
         L_Name := Name;
      end Set_Name;
   end Light;
end overriding_ops;
