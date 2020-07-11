/* { dg-do run } */
/* { dg-require-effective-target s390_vxe2 } */
/* { dg-options "-O3 -mzarch -march=arch13 -mzvector --save-temps" } */

#include <string.h>
#include <vecintrin.h>

typedef vector unsigned char uv16qi;

const unsigned char test_vec[16] = { 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16 };

#define NUM_TEST_LENGTHS 3

unsigned int test_len[NUM_TEST_LENGTHS] = { 0, 12, 18 };


/* Proceeding from left to right, the specified number (LEN+1) of
   bytes from SOURCE are stored right-justified in TARGET.  */
void __attribute__((noinline, noclone, target ("arch=zEC12")))
emul (const unsigned char *source, unsigned char *target, unsigned int len)
{
  int start = 15 - len;
  if (start < 0)
    start = 0;
  for (int s = 0, t = start; t < 16; s++, t++)
    target[t] = source[s];
}

uv16qi __attribute__((noinline, noclone))
vec_load_len_r_reg (const unsigned char *s, unsigned int len)
{
  return vec_load_len_r (s, len);
}

void __attribute__((noinline, noclone))
vec_load_len_r_mem (const unsigned char *s, uv16qi *t, unsigned int *len)
{
  *t = vec_load_len_r (s, *len);
}

#define GEN_CONST_FUNC(CONST)				\
  static uv16qi inline						\
  vec_load_len_r_const##CONST (const unsigned char *s)	\
  {							\
    return vec_load_len_r (s, CONST);			\
  }

#define GEN_CONST_TEST(CONST)				\
  memset (exp_result, 0, 16);				\
  emul (test_vec, exp_result, CONST);			\
  result = (uv16qi) { 0 };				\
  result = vec_load_len_r_const##CONST (test_vec);	\
  if (memcmp ((char*)&result, exp_result, 16) != 0)	\
    __builtin_abort ();

GEN_CONST_FUNC(0)
GEN_CONST_FUNC(12)
GEN_CONST_FUNC(18)

int
main ()
{
  unsigned char exp_result[16];
  uv16qi result;

  for (int i = 0; i < NUM_TEST_LENGTHS; i++)
    {
      memset (exp_result, 0, 16);

      emul (test_vec, exp_result, test_len[i]);

      result = (uv16qi) { 0 };
      result = vec_load_len_r_reg (test_vec, test_len[i]);
      if (memcmp ((char*)&result, exp_result, 16) != 0)
	__builtin_abort ();

      result = (uv16qi) { 0 };
      vec_load_len_r_mem (test_vec, &result, &test_len[i]);
      if (memcmp ((char*)&result, exp_result, 16) != 0)
	__builtin_abort ();
    }

  GEN_CONST_TEST(0)
  GEN_CONST_TEST(12)
  GEN_CONST_TEST(18)

  return 0;
}

/* vec_load_len_r_reg and vec_load_len_r_mem */
/* { dg-final { scan-assembler-times "vlrlr\t" 2 } } */

/* For the 2 constants.  The 3. should be implemented with vl.  */
/* { dg-final { scan-assembler-times "vlrl\t" 2 } } */
