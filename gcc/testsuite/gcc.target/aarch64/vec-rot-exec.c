/* { dg-do run } */
/* { dg-options "-O2" } */

typedef char __attribute__ ((vector_size (16))) v16qi;
typedef unsigned short __attribute__ ((vector_size (16))) v8hi;
typedef unsigned int __attribute__ ((vector_size (16))) v4si;
typedef unsigned long long __attribute__ ((vector_size (16))) v2di;
typedef char __attribute__ ((vector_size (8))) v8qi;
typedef unsigned short __attribute__ ((vector_size (8))) v4hi;
typedef unsigned int __attribute__ ((vector_size (8))) v2si;
#define VEC_ELTS(X) (sizeof (X) / (sizeof (X[0])))

static const char __attribute__ ((aligned (16))) *str = "abcdefghijklmnopqrstuvwxyz";

unsigned long long
__attribute__((noipa,noinline)) 
rot_64_one (unsigned long long x, unsigned amt)
{
  return (x << amt) | (x >> (64 - amt));
}
unsigned int
__attribute__((noipa,noinline)) 
rot_32_one (unsigned int x, unsigned amt)
{
  return (x << amt) | (x >> (32 - amt));
}

unsigned short
__attribute__((noipa,noinline)) 
rot_16_one (unsigned short x, unsigned short amt)
{
  return (x << amt) | (x >> (16 - amt));
}


#define ROTFUNC(M,S,A)					\
M							\
__attribute__((noipa,noinline)) 			\
rot_##M##_##S##_##A (M x)				\
{							\
  return (x << A) | (x >> (S - A));			\
}							\
							\
void							\
test_rot_##M##_##S##_##A (void)				\
{							\
  M vec = *(M *)str;					\
  M res = rot_##M##_##S##_##A (vec);			\
  for (__SIZE_TYPE__ i = 0; i < VEC_ELTS (vec); i++)	\
    if (res[i] != rot_##S##_one (vec[i], A))		\
      __builtin_abort ();				\
}

ROTFUNC (v2di, 64, 56)
ROTFUNC (v2di, 64, 48)
ROTFUNC (v2di, 64, 40)
ROTFUNC (v2di, 64, 32)
ROTFUNC (v2di, 64, 24)
ROTFUNC (v2di, 64, 16)
ROTFUNC (v2di, 64, 8)

ROTFUNC (v4si, 32, 24)
ROTFUNC (v4si, 32, 16)
ROTFUNC (v4si, 32, 8)

ROTFUNC (v8hi, 16, 8)

ROTFUNC (v2si, 32, 24)
ROTFUNC (v2si, 32, 16)
ROTFUNC (v2si, 32, 8)

ROTFUNC (v4hi, 16, 8)

#define CALL_TEST(M,S,A) test_rot_##M##_##S##_##A ()

int
main (void)
{
  CALL_TEST (v2di, 64, 56);
  CALL_TEST (v2di, 64, 48);
  CALL_TEST (v2di, 64, 40);
  CALL_TEST (v2di, 64, 32);
  CALL_TEST (v2di, 64, 24);
  CALL_TEST (v2di, 64, 16);
  CALL_TEST (v2di, 64, 8);

  CALL_TEST (v4si, 32, 24);
  CALL_TEST (v4si, 32, 16);
  CALL_TEST (v4si, 32, 8);

  CALL_TEST (v8hi, 16, 8);

  CALL_TEST (v2si, 32, 24);
  CALL_TEST (v2si, 32, 16);
  CALL_TEST (v2si, 32, 8);

  CALL_TEST (v4hi, 16, 8);

  return 0;
}

