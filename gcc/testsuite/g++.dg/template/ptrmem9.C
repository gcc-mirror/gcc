// PR c++/15329

struct S {}; 
 
template <typename> struct X { 
    S s; 
    void foo (void (S::*p)()) 
      { (s.*p)(); } 
}; 
