package body Aggr21_Pkg is

   procedure Init (R : out Rec) is
   begin
      R := (A => 5, S => <>, N => 7);
   end;

end Aggr21_Pkg;
