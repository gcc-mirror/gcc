package body Limited1_Inner is
   overriding procedure Finalize (X : in out Limited_Type) is
   begin
      if X.Self /= X'Unchecked_Access then
         raise Program_Error with "Copied!";
      end if;
   end;

   function Make_Inner return Inner_Type is
   begin
      return Inner : Inner_Type (True) do
         null;
      end return;
   end;
end;
