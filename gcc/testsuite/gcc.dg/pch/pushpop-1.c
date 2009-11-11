#include "pushpop-1.hs"

#if FOO != 2
#error FOO != 2
#endif
#pragma pop_macro("FOO")

#if FOO != 1
#error FOR != 1
#endif

