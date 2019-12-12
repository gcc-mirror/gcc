package body Default_Initial_Condition_Pack is
   function Is_OK (Val : T) return Boolean is
   begin
      DIC_Called := True;
      return True;
   end Is_OK;
end Default_Initial_Condition_Pack;
