/* { dg-do compile { target { powerpc*-*-linux* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx -O2" } */
/* { dg-final { scan-assembler-times "bcdadd\[.\] " 4 } } */
/* { dg-final { scan-assembler-times "bcdsub\[.\] " 6 } } */
/* { dg-final { scan-assembler-not   "bl __builtin"   } } */
/* { dg-final { scan-assembler-not   "mtvsr"   	      } } */
/* { dg-final { scan-assembler-not   "mfvsr"   	      } } */
/* { dg-final { scan-assembler-not   "lvx"     	      } } */
/* { dg-final { scan-assembler-not   "lxvw4x"  	      } } */
/* { dg-final { scan-assembler-not   "lxvd2x"  	      } } */
/* { dg-final { scan-assembler-not   "stvx"    	      } } */
/* { dg-final { scan-assembler-not   "stxvw4x" 	      } } */
/* { dg-final { scan-assembler-not   "stxvd2x" 	      } } */

typedef __int128_t __attribute__((__vector_size__(16)))	vector_128_t;
typedef __int128_t					scalar_128_t;
typedef	unsigned long long				scalar_64_t;

#include <altivec.h>

/* Test whether the peephole works to allow folding a bcdadd, with a
   bcdadd_<test> into a single instruction.  */

vector_128_t
do_add_lt (vector_128_t a, vector_128_t b, int *p)
{
  vector_128_t ret = __builtin_bcdadd (a, b, 0);
  if (__builtin_bcdadd_lt (a, b, 0))
    *p = 1;

  return ret;
}

vector_128_t
do_add_eq (vector_128_t a, vector_128_t b, int *p)
{
  vector_128_t ret = __builtin_bcdadd (a, b, 0);
  if (__builtin_bcdadd_eq (a, b, 0))
    *p = 1;

  return ret;
}

vector_128_t
do_add_gt (vector_128_t a, vector_128_t b, int *p)
{
  vector_128_t ret = __builtin_bcdadd (a, b, 0);
  if (__builtin_bcdadd_gt (a, b, 0))
    *p = 1;

  return ret;
}

vector_128_t
do_add_ov (vector_128_t a, vector_128_t b, int *p)
{
  vector_128_t ret = __builtin_bcdadd (a, b, 0);
  if (__builtin_bcdadd_ov (a, b, 0))
    *p = 1;

  return ret;
}

vector_128_t
do_sub_lt (vector_128_t a, vector_128_t b, int *p)
{
  vector_128_t ret = __builtin_bcdsub (a, b, 0);
  if (__builtin_bcdsub_lt (a, b, 0))
    *p = 1;

  return ret;
}

vector_128_t
do_sub_eq (vector_128_t a, vector_128_t b, int *p)
{
  vector_128_t ret = __builtin_bcdsub (a, b, 0);
  if (__builtin_bcdsub_eq (a, b, 0))
    *p = 1;

  return ret;
}

vector_128_t
do_sub_gt (vector_128_t a, vector_128_t b, int *p)
{
  vector_128_t ret = __builtin_bcdsub (a, b, 0);
  if (__builtin_bcdsub_gt (a, b, 0))
    *p = 1;

  return ret;
}

vector_128_t
do_sub_ge (vector_128_t a, vector_128_t b, int *p)
{
  vector_128_t ret = __builtin_bcdsub (a, b, 0);
  if (__builtin_bcdsub_ge (a, b, 0))
    *p = 1;

  return ret;
}

vector_128_t
do_sub_le (vector_128_t a, vector_128_t b, int *p)
{
  vector_128_t ret = __builtin_bcdsub (a, b, 0);
  if (__builtin_bcdsub_le (a, b, 0))
    *p = 1;

  return ret;
}

vector_128_t
do_sub_ov (vector_128_t a, vector_128_t b, int *p)
{
  vector_128_t ret = __builtin_bcdsub (a, b, 0);
  if (__builtin_bcdsub_ov (a, b, 0))
    *p = 1;

  return ret;
}
