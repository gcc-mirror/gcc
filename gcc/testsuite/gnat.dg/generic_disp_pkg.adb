package body Generic_Disp_Pkg is

   function Constructor (I : not null access Integer) return DT is
      R : DT; 
   begin
      return R;
   end Constructor;

end Generic_Disp_Pkg;
