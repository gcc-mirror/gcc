with Elab7_Pkg1;

package body Elab7_Pkg2 is
   procedure From_Timerep is
      Lf1 : Long_Float := 1.0;
      Lf2 : Long_Float := Long_Float'Floor(Lf1);
   begin
      null;
   end From_Timerep;

   procedure A is
   begin
      Elab7_Pkg1.A;
   end A;
end Elab7_Pkg2;
