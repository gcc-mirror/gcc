package body Opt48_Pkg1 is

   function G return Rec is
   begin
      return (32, "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA");
   end G;

   X : Rec := F;
   Y : Rec := G;
   Z : Rec := F;

   function Get_Z return Rec is
   begin
      return Z;
   end;

end Opt48_Pkg1;
