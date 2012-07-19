package body Opt25_Pkg1 is

   procedure Swap (A, B : in out T) is
      Tmp : T := A;
   begin
      A := B;
      B := Tmp;
   end Swap;

end Opt25_Pkg1;
