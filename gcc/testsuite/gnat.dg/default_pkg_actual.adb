--  { dg-do compile }

procedure Default_Pkg_Actual is

   generic
   package As is
   end As;

   generic
      type T is private;
      with package A0 is new As;
   package Bs is
   end Bs;

   generic
      with package Xa is new As;
   package Xs is
      package Xb is new Bs(T => Integer, A0 => Xa);
   end Xs;

   generic
      with package Yb is new Bs(T => Integer, others => <>);
   package Ys is
   end Ys;

   package A is new As;
   package X is new Xs(Xa => A);
   package Y is new Ys(Yb => X.Xb);

begin
   null;
end;
