package body BIP_CU_T is

   function Make_T (Name : String) return T is
   begin
      return (Name => To_Unbounded_String (Name), others => <>);
   end Make_T;

end BIP_CU_T;
