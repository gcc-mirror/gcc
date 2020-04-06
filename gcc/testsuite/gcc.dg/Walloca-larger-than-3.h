#if __has_include (<alloca.h>)
#  include <alloca.h>
#endif

#ifndef alloca
   /* Simulate a definition in a system header. */
#  pragma GCC system_header
#  define alloca(n) __builtin_alloca (n)
#endif
