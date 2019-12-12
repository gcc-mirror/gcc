package body Addr12_B is

   function Initial_State return Shared_Context_Type is
   begin
      return Shared_Context_Type'(Data => (others => Null_Entry));
   end Initial_State;

end Addr12_B;
