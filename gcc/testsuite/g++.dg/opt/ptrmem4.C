// { dg-do compile }
// { dg-options "-O3" }

struct X { void foo (); }; 
 
template <typename> 
inline void spawn (void (X::*fun_ptr)()) {} 
 
void bar () { 
  void (X::*const comp)() = &X::foo; 
  spawn<int> (comp); 
} 
