package body Discr49_Rec2 is
   function Value (Obj : Child) return Integer is
   begin
      return Value (Parent (Obj));
   end;
end Discr49_Rec2;
