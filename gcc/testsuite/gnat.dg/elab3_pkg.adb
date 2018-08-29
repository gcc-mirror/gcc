package body Elab3_Pkg is
   procedure Elaborator is
   begin
      Proc;
   end Elaborator;

begin
   if Elaborate then
      Elaborator;
   end if;
end Elab3_Pkg;
