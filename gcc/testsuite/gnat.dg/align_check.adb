-- { dg-do run }

with System; 
procedure align_check is
   N_Allocated_Buffers : Natural := 0;
--      
   function New_Buffer (N_Bytes : Natural) return System.Address is
   begin   
      N_Allocated_Buffers := N_Allocated_Buffers + 1;
      return System.Null_Address;
   end;    
--      
   Buffer_Address : constant System.Address := New_Buffer (N_Bytes => 8);
   N : Natural;
   for N'Address use Buffer_Address;
--      
begin   
   if N_Allocated_Buffers /= 1 then
      raise Program_Error;
   end if; 
end;    
