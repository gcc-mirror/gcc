package Global
  with Abstract_State => (State with External)
is
   protected type Prot_Typ is
      procedure Force_Body;
   end Prot_Typ;

   protected Prot_Obj is
      procedure Force_Body;
   end Prot_Obj;

   task type Task_Typ is
      entry Force_Body;
   end Task_Typ;

   task Task_Obj is
      entry Force_Body;
   end Task_Obj;
end Global;
