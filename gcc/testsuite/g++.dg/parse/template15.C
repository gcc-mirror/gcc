// { dg-do compile }
// Contributed by: Peter Schmid 
//   <schmid at snake dot iap dot physik dot tu-darmstadt dot de>
// PR c++/14545: constructor calls are not integer constant expressions

struct A1 { A1(); }; 
struct A2 { }; 

template <class T> 
struct B
{ 
  void foo() { 
    A1();
    A1 a1 = A1(); 

    A2();
    A2 a2 = A2(); 

    int();
    int a3 = int();
    float();
    float a4 = float();
  } 
}; 

template struct B<void>;
