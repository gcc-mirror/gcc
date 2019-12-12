// PR c++/89966
// { dg-do compile { target c++17 } }

template < auto a0 >
void f0() { }
void f0_call() { f0< sizeof(int) >(); }
