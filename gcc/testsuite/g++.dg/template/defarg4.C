// PR c++/14763

struct A { 
  int get() const {} 
  static A *foo(); 
}; 
 
template<bool> struct S { 
  S(unsigned int = A::foo()->get())  ; 
}; 
 
void foo() throw() { 
  S<false> f; 
} 
