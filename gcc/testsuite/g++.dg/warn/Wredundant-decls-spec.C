// PR c++/15867
// { dg-options -Wredundant-decls }

template <typename T> struct S
{  
  void foo() {}          
};  

template<> void S<int>::foo();

template<> void S<double>::foo();  // { dg-warning "previous declaration" }
template<> void S<double>::foo();  // { dg-warning "redundant redeclaration" }
