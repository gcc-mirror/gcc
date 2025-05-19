// PR c++/120350
// { dg-additional-options "-fmodule-header" }
// { dg-module-cmi {} }

#include <typeinfo>
struct S {};
inline const std::type_info& tinfo = typeid(S);
