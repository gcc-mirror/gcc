package Expr_Func3 is

   type Obj_T is abstract tagged null record;

   type T is access all Obj_T'Class;

   function Slave (Obj : access Obj_T) return T is (T(Obj));

   function Optional_Slave (Obj : T) return T;

   procedure Dummy;

private

   function Optional_Slave (Obj : T) return T is
    (if Obj = null then null else Slave (Obj));

end Expr_Func3;
