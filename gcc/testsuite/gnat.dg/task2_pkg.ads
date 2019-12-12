package Task2_Pkg is
   type T is task Interface;
   task type T2 is new T with end;
end Task2_pkg;
