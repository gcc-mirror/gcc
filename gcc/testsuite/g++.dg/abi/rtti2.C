// { dg-do run }

#include <cxxabi.h>
#include <typeinfo>

int main () {
  const std::type_info& ti = typeid (const int (*)[3]);
  const abi::__pointer_type_info& pti 
    = static_cast<const abi::__pointer_type_info&>(ti);
  if ((pti.__flags & pti.__const_mask) == 0)
    return 1;
}
