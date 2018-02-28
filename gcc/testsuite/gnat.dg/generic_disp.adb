--  { dg-do run }

with Generic_Disp_Pkg; use Generic_Disp_Pkg;

procedure Generic_Disp is
   I : aliased Integer := 0;
   D : Iface'Class := Dispatching_Constructor (DT'Tag, I'access);
begin   
   null;   
end Generic_Disp;
