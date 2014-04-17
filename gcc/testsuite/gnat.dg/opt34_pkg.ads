package Opt34_Pkg is

   type T_Private is record
      I : Integer := 0;
   end record;

   T_Private_Zero : constant T_Private := (I => 0);

   function Get_Private (I : Integer) return T_Private;
   function Get_Integer (X : T_Private) return Integer;

   procedure Assert (Cond : Boolean);

end Opt34_Pkg;
