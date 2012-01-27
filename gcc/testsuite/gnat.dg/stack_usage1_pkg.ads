package Stack_Usage1_Pkg is

   function Ident_Int (X : Integer) return Integer;

   type R is
       record
          C0, C1, C2, C3, C4, C5, C6, C7, C8, C9 : Integer;
       end record;

   procedure My_Proc (X : R);

end Stack_Usage1_Pkg;
