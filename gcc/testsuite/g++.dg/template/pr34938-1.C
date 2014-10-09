// PR c++/34938

typedef void (*fptr)() __attribute((noreturn)); 
template<int>  void foo();
template<fptr> void bar();

fptr f = bar< foo<0> >;   // { dg-error "noreturn" }
