/* { dg-do compile { target { powerpc*-*-linux* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-skip-if "" { powerpc*-*-*spe* } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -O2" } */
/* { dg-final { scan-assembler-times "bcdadd\[.\] " 2 } } */
/* { dg-final { scan-assembler-times "bcdsub\[.\] " 2 } } */
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

vector_128_t
do_add_0 (vector_128_t a, vector_128_t b)
{
  return __builtin_bcdadd (a, b, 0);
}

vector_128_t
do_add_1 (vector_128_t a, vector_128_t b)
{
  return __builtin_bcdadd (a, b, 1);
}

vector_128_t
do_sub_0 (vector_128_t a, vector_128_t b)
{
  return __builtin_bcdsub (a, b, 0);
}

vector_128_t
do_sub_1 (vector_128_t a, vector_128_t b)
{
  return __builtin_bcdsub (a, b, 1);
}
