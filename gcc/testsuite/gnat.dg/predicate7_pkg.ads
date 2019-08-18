package Predicate7_Pkg is
  subtype My_Int is Integer with Dynamic_Predicate => My_Int /= 0;
end Predicate7_Pkg;
