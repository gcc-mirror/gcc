package body Opt96_Pkg is

   function F (D : Data) return Integer is
      X : constant Long_Float := Long_Float (D.Foo.Bar.X);
      Y : constant Long_Float := Long_Float (D.Foo.Bar.Y);
   begin
      return Integer ((X * 1000.0) + (Y * 1000.0));
   end;

   function F (Self : Rec; D  : Data'Class) return Integer is
      Base_Data : constant Data := Data (D);
   begin
      return F (Base_Data);
   end;

end Opt96_Pkg;
