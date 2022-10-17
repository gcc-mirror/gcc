/* { dg-require-effective-target int128     } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2" } */

/* PR target/104698 involved a regression where on power10, conversion from
   long long to __int128_t generated mtvsrdd, vextsd2q, mfvsrd, and mfvsrld
   instructions instead of just a GPR sign extension.  This test makes sure the
   result is kept in the GPR registers.  */

__int128_t convert_1 (long long a)
{
  return a;	/* sradi.  */
}

/* Like convert_1, but make sure a normal offsettable load is done.  The
   pattern in vsx.md has support for generating lxvdsx if it is coming from
   memory.  Make sure when the gpr is used, a normal load with offset is still
   done.  */

__int128_t convert_2 (long long *p)
{
  return p[2];	/* ld and sradi.  */
}

/* { dg-final { scan-assembler-not   {\mmfvsrd\M}     } } */
/* { dg-final { scan-assembler-not   {\mmfvsrld\M}    } } */
/* { dg-final { scan-assembler-not   {\mmtvsrdd\M}    } } */
/* { dg-final { scan-assembler-not   {\mvextsd2q\M}   } } */
/* { dg-final { scan-assembler-times {\mld\M}       1 } } */
/* { dg-final { scan-assembler-times {\msradi\M}    2 } } */
