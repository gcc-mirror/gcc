package Lto18_Pkg is

   function N return Positive;
   pragma Import (Ada, N);

   type Path is array(1 .. N) of Long_Float;
   type Path_Vector is array (Positive range <>) of Path;
   type Path_Vector_P is access all Path_Vector;
   type Path_Vector_PV is array(Positive range <>) of Path_Vector_P;
   type Path_Vector_P2 is access all Path_Vector_PV;

   type Vector is array (Positive range <>) of Natural;
   type Vector_Access is access Vector;

   type Rec is record
      Val  : Path_Vector_P2;
      Step : Vector_Access;
   end record;

   function Get (R : Rec; I : Positive; M : Natural) return Path;
--   pragma Inline (Get);

end Lto18_Pkg;
