--  { dg-do compile }

package body No_Caching with SPARK_Mode is
   Status : Boolean;

   procedure Handle (V : Mult_Bit_Boolean) is
      Ret_Val : Mult_Bit_Boolean := V with Volatile, No_Caching;
   begin
      if (Ret_Val = NV_TRUE) then
         Do_Something;
      elsif (Ret_Val = NV_FALSE) then
         Do_Something_Else;
      else
         null;
         -- Fault inject detected. Take punitive action
      end if;
   end Handle;

   procedure Do_Something is
   begin
      Status := True;
   end Do_Something;

   procedure Do_Something_Else is
   begin
      Status := False;
   end Do_Something_Else;

end No_Caching;
