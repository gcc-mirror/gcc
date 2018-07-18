package Subp_Inst_Pkg is
   pragma Pure;

   generic
      type T;
      type T_Access is access T;
   function Image (Val : T_Access) return String;

   generic
      type T;
   function T_Image (Val : access T) return String;

end Subp_Inst_Pkg;
