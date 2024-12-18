// { dg-do compile { target arm_eabi } }
// { dg-options "-O2" } 
// Check that, even when optimizing, we emit an out-of-line call to
// the type-info comparison function.
// { dg-final { scan-assembler _ZNKSt9type_infoeqERKS_ { target { ! c++26 } } } }
// { dg-final { scan-assembler _ZNKSt9type_info7__equalERKS_ { target { c++26 } } } }

#include <typeinfo>

extern const std::type_info& t1;
extern const std::type_info& t2;

bool f() {
  return t1 == t2;
}
