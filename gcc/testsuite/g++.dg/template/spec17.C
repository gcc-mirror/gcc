// PR c++/16224

namespace io { 
  template <typename> int foo(); // { dg-error "" }
} 
 
using namespace io; 
 
template<> int foo<int>(); // { dg-error "" }
 
int a = foo<int>(); 
