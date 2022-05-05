/* { dg-require-effective-target int128     } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

/* PR target/104694 involved GCC generating vextsd2q to convent long long to
   __int128_t when the long long value was in the GPR register.  This test
   verifies that if the result is in the Altivec registers, we still want to
   generate vextsd2q.  We use __int128_t to indicate that we want the result of
   the conversion to be in an Altivec register. */

void do_div_1 (__int128_t *p, __int128_t *q, long long r)
{
  *p = *q / r;		/* mtvsrdd, vextsd2q, vdivsq.  */
}

/* Test the optimization in vsx.md to use lxvrdx instead of ld and mtvsrdd if
   the value is coming from memory.  */

void do_div_2 (__int128_t *p, __int128_t *q, long long *r)
{
  *p = *q / r[2];	/* lxvrdx, vextsd2q, vdivsq.  */
}

/* { dg-final { scan-assembler-not   {\mld\M}         } } */
/* { dg-final { scan-assembler-not   {\mmfvsrd\M}     } } */
/* { dg-final { scan-assembler-not   {\mmfvsrld\M}    } } */
/* { dg-final { scan-assembler-not   {\msradi\M}      } } */
/* { dg-final { scan-assembler-times {\mlxv\M}      2 } } */
/* { dg-final { scan-assembler-times {\mlxvrdx\M}   1 } } */
/* { dg-final { scan-assembler-times {\mmtvsrdd\M}  1 } } */
/* { dg-final { scan-assembler-times {\mstxv\M}     2 } } */
/* { dg-final { scan-assembler-times {\mvdivsq\M}   2 } } */
/* { dg-final { scan-assembler-times {\mvextsd2q\M} 2 } } */
