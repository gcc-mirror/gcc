package Expr_Func2 is

   type T_Index is range 1 .. 255;

   type T_Table is array (T_Index range <>) of Boolean;

   type T_Variable_Table (N : T_Index := T_Index'First) is record
      Table : T_Table (1 .. N);
   end record;

   type T_A_Variable_Table is access T_Variable_Table;

   function Element (A_Variable_Table : T_A_Variable_Table) return Boolean;

private

   function Element (A_Variable_Table : T_A_Variable_Table) return Boolean is
     (A_Variable_Table.all.Table (1));

   procedure Foo;

end Expr_Func2;
