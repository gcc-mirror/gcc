package body Discr49_Rec1 is
   function Value (Obj : Parent) return Integer is
   begin
      return Obj.V + Boolean'Pos (Obj.Discr_1);
   end;
end Discr49_Rec1;
