/* { dg-do run } */
/* { dg-require-effective-target s390_vxe2 } */
/* { dg-options "-O3 -mzarch -march=arch13 -mzvector --save-temps" } */

#include <string.h>
#include <vecintrin.h>

typedef vector unsigned char uv16qi;

uv16qi test_vec = (uv16qi){ 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16 };

#define NUM_TEST_LENGTHS 3

unsigned int test_len[NUM_TEST_LENGTHS] = { 0, 12, 18 };


/* Proceeding from left to right, the specified number (LEN+1) of
   rightmost bytes from SOURCE are stored in TARGET.  */
void __attribute__((noinline, noclone, target ("arch=zEC12")))
emul (unsigned char *source, unsigned char *target, unsigned int len)
{
  int start = 15 - len;
  if (start < 0)
    start = 0;
  for (int s = start, t = 0; s < 16; s++, t++)
    target[t] = source[s];
}

void __attribute__((noinline, noclone))
vec_store_len_r_reg (uv16qi s, unsigned char *t, unsigned int len)
{
  vec_store_len_r (s, t, len);
}

void __attribute__((noinline, noclone))
vec_store_len_r_mem (uv16qi *s, unsigned char *t, unsigned int *len)
{
  vec_store_len_r (*s, t, *len);
}

#define GEN_CONST_FUNC(CONST)					\
  static void inline						\
  vec_store_len_r_const##CONST (uv16qi s, unsigned char *t)	\
  {								\
    vec_store_len_r (s, t, CONST);				\
  }

#define GEN_CONST_TEST(CONST)					\
  memset (exp_result, 0, 16);					\
  emul ((unsigned char*)&test_vec, exp_result, CONST);		\
  memset (result, 0, 16);					\
  vec_store_len_r_const##CONST (test_vec, result);		\
  if (memcmp (result, exp_result, 16) != 0)			\
    __builtin_abort ();

GEN_CONST_FUNC(0)
GEN_CONST_FUNC(12)
GEN_CONST_FUNC(18)

int
main ()
{
  unsigned char exp_result[16];
  unsigned char result[16];

  for (int i = 0; i < NUM_TEST_LENGTHS; i++)
    {
      memset (exp_result, 0, 16);

      emul ((unsigned char*)&test_vec, exp_result, test_len[i]);

      memset (result, 0, 16);
      vec_store_len_r_reg (test_vec, result, test_len[i]);
      if (memcmp (result, exp_result, 16) != 0)
	__builtin_abort ();

      memset (result, 0, 16);
      vec_store_len_r_mem (&test_vec, result, &test_len[i]);
      if (memcmp (result, exp_result, 16) != 0)
	__builtin_abort ();
    }

  GEN_CONST_TEST(0)
  GEN_CONST_TEST(12)
  GEN_CONST_TEST(18)

  return 0;
}

/* vec_store_len_r_reg and vec_store_len_r_mem */
/* { dg-final { scan-assembler-times "vstrlr\t" 2 } } */

/* For the 2 constants.  The 3. should be implemented with vst.  */
/* { dg-final { scan-assembler-times "vstrl\t" 2 } } */
