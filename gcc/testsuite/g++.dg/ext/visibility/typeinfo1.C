// PR c++/26984
// lazily generated typeinfos should not be affected by #pragma vis, but
// they should be affected by the visibility of the type they describe.

// { dg-require-visibility "" }
// { dg-options "-fvisibility-inlines-hidden" }
// { dg-final { scan-not-hidden "_ZTIPPi" } }
// { dg-final { scan-not-hidden "_ZTSPPi" } }
// { dg-final { scan-hidden "_ZTIP1A" } }
// { dg-final { scan-hidden "_ZTSP1A" } }

#include <typeinfo>

#pragma GCC visibility push(hidden)
const std::type_info* t = &(typeid(int **));
struct A { };
#pragma GCC visibility pop

const std::type_info* t2 = &(typeid(A *));
