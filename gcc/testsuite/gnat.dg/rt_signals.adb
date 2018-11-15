--  { dg-do run }

--  This program used to fail with a runtime built with assertions

procedure RT_Signals is
   task Task_A;

   task body Task_A is
   begin
      null;
   end Task_A;
begin
   null;
end RT_Signals;
