--  { dg-do compile }

with System.Multiprocessors;

procedure Task4 is

  task type Infinite_Loop (C : System.Multiprocessors.CPU_Range)
     with CPU => C;

  task body Infinite_Loop is
  begin
     loop
        null;
     end loop;
  end Infinite_Loop;

begin
  null;
end Task4;
