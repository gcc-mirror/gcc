package Task1_Pkg is
   subtype Typ_Bool is boolean;

   type Typ_Task_Par is record
      Dummy : Typ_Bool;
   end record;

   type Typ_Task_Par_Tab is array (1 .. 33) of aliased Typ_Task_Par;
   task type Typ_Task (dummy : Typ_Bool);
end Task1_Pkg;
