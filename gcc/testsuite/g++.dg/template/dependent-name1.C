// { dg-do compile }

// Origin: Wolfgang Bangerth <bangerth@ticam.utexas.edu>

// PR c++/10347: Dependent type checking of array new expression

void bar (int *);
    
template <int> void foo() {
  bar(new int[1]);
}
