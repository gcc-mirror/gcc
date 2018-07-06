package body Limited1_Outer is
   function Make_Outer return Outer_Type is
   begin
      return (What => True, Inner => Make_Inner);
   end;
end;
