/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-O2 -mavoid-indexed-addresses -mno-altivec -mno-vsx" } */

/* { dg-final { scan-assembler-not "lbzx" } }

/* Ensure that an indexed load is not generated with
   -mavoid-indexed-addresses. */

char
do_one (char *base, unsigned long offset)
{
  return base[offset];
}

