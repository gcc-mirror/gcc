// PR c++/13592

struct S { 
  void operator()(int); 
}; 
 
struct A { 
  template <typename> void foo(); 
  S s; 
}; 
 
template <typename> void A::foo() { 
  s(0); 
} 
