package Discr49_Rec1 is
   type Parent (Discr_1 : Boolean; Discr_2 : Boolean) is private;
   function Value (Obj : Parent) return Integer;
private
   type Parent (Discr_1 : Boolean; Discr_2 : Boolean) is record
      V : Integer := 123;
   end record;
end Discr49_Rec1;
