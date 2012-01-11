package body Opt23_Pkg is

   function Get (R : Rec; I : Positive; M : Natural) return Path is
   begin
      return R.Val (I) (M);
   end;

end Opt23_Pkg;
