#include "harness.h"

vector unsigned char
f (vector unsigned char a, vector unsigned char b, vector unsigned char c)
{
#ifdef __BIG_ENDIAN__
  return vec_perm(a,b,c); 
#else
  return vec_perm(b,a,c);
#endif
}

static void test()
{
  check(vec_all_eq(f(((vector unsigned char){0,1,2,3,4,5,6,7,
					    8,9,10,11,12,13,14,15}),
		     ((vector unsigned char){70,71,72,73,74,75,76,77,
					    78,79,80,81,82,83,84,85}),
#ifdef __BIG_ENDIAN__
		     ((vector unsigned char){0x1,0x14,0x18,0x10,0x16,0x15,0x19,0x1a,
					    0x1c,0x1c,0x1c,0x12,0x8,0x1d,0x1b,0xe})),
#else
                     ((vector unsigned char){0x1e,0xb,0x7,0xf,0x9,0xa,0x6,0x5,
                                            0x3,0x3,0x3,0xd,0x17,0x2,0x4,0x11})),
#endif
		   ((vector unsigned char){1,74,78,70,76,75,79,80,82,82,82,72,8,83,81,14})),
	"f");
}
