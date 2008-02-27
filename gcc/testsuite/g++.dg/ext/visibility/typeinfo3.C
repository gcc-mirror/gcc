// PR c++/35368
// { dg-require-visibility "" }

#pragma GCC visibility push (hidden)

#include <typeinfo>

const std::type_info& info1 = typeid(int []);
const std::type_info& info2 = typeid(int);
enum E { e = 0 };
const std::type_info& info3 = typeid(E);
struct S { S (); };
const std::type_info& info4 = typeid(S);
const std::type_info& info5 = typeid(int *);

// { dg-final { scan-not-hidden "_ZTVN10__cxxabiv117__array_type_infoE" } }
// { dg-final { scan-not-hidden "_ZTVN10__cxxabiv116__enum_type_infoE" } }
// { dg-final { scan-hidden "_ZTI1S" } }
// { dg-final { scan-hidden "_ZTS1S" } }
// { dg-final { scan-hidden "info1" } }
// { dg-final { scan-hidden "info2" } }
// { dg-final { scan-hidden "info3" } }
// { dg-final { scan-hidden "info4" } }
// { dg-final { scan-hidden "info5" } }
