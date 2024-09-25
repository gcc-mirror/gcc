#include <immintrin.h>
/*  In file included from /usr/src/local/gcc/obj/gcc/include/emmintrin.h:31,
    from /usr/src/local/gcc/obj/x86_64-unknown-netbsd10.99/libstdc++-v3/include/ext/random:45,
    from /usr/src/local/gcc/libstdc++-v3/include/precompiled/extc++.h:65:
    /usr/src/local/gcc/obj/gcc/include/xmmintrin.h:75:15: error: conflicting declaration 'typedef float __float_u'
    75 | typedef float __float_u __attribute__ ((__may_alias__, __aligned__ (1)));
    |               ^~~~~~~~~
    In file included from /usr/src/local/gcc/obj/x86_64-unknown-netbsd10.99/libstdc++-v3/include/cmath:47,
    from /usr/src/local/gcc/obj/x86_64-unknown-netbsd10.99/libstdc++-v3/include/x86_64-unknown-netbsd10.99/bits/stdc++.h:114,
    from /usr/src/local/gcc/libstdc++-v3/include/precompiled/extc++.h:32:
    /usr/src/local/gcc/obj/gcc/include-fixed/math.h:49:7: note: previous declaration as 'union __float_u'
    49 | union __float_u {  */
typedef union {
  float a;
  char b[4];
}__float_u;

char
foo (float a)
{
  __float_u c;
  c.a = a;
  return c.b[1];
}
