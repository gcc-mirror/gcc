package body Pack20 is

   procedure Proc (A : Rec) is
      Local : Rec := A;
   begin
      Modify (Local.Fixed);
   end;

end Pack20;
