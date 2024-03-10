#include <altivec.h>

#ifdef __BIG_ENDIAN__
#define LANE 1
#else
#define LANE 2
#endif

unsigned int foo1 (vector unsigned int v)
{
   return vec_extract(v, LANE);
}

void foo2 (vector unsigned int v, unsigned int* p)
{
   *p = vec_extract(v, LANE);
}
