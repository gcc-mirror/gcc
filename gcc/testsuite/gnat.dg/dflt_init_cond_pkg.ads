package Dflt_Init_Cond_Pkg is
   type Explicit is limited private with Default_Initial_Condition => True;
   type Implicit is limited private with Default_Initial_Condition;

   procedure Read (Obj : Explicit);
   procedure Read (Obj : Implicit);

private
   type Implicit is access all Integer;
   type Explicit is access all Integer;
end Dflt_Init_Cond_Pkg;
