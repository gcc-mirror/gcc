// { dg-options "-std=c++0x" }
// PR c++/34751
struct A {};

template<typename... Args = int>  // { dg-error "cannot have a default" }
void f(Args... args = 0); // { dg-error "cannot have a default argument" }

template<typename... Args> 
void g(Args... = 0); // { dg-error "cannot have a default argument" }


template<int, typename T = A, int T::*...p = 0 > struct B {}; // { dg-error "cannot have a default argument|no default argument" }

B<0> b;

template<int, typename T = A, int T::*... = 0 > struct C {}; // { dg-error "cannot have a default argument|no default argument" }

C<0> c;

