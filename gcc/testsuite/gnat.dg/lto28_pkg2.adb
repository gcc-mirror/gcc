package body Lto28_Pkg2 is

   function F return Lto28_Pkg3.Q_Rec is
   begin
      return Result : Lto28_Pkg3.Q_Rec := Lto28_Pkg3.Default_Q_Rec do
         Result.A := 1.0;
      end return;
   end;

end Lto28_Pkg2;
