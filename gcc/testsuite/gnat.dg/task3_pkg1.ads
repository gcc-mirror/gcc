package Task3_Pkg1 is
   type Task_Wrapper (Discr : Integer) is tagged limited private;

private
   task type Task_Typ (Discr : Integer) is
   end Task_Typ;

   type Task_Wrapper (Discr : Integer) is tagged limited record
      Tsk : Task_Typ (Discr);
   end record;
end Task3_Pkg1;
