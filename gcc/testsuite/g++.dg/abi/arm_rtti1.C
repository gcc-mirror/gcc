// { dg-do compile { target arm*-*-eabi* arm*-*-symbianelf* } }
// { dg-options "-O2" } 
// Check that, even when optimizing, we emit an out-of-line call to
// the type-info comparison function.
// { dg-final { scan-assembler _ZNKSt9type_infoeqERKS_ } }

#include <typeinfo>

extern const std::type_info& t1;
extern const std::type_info& t2;

bool f() {
  return t1 == t2;
}
