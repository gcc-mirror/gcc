/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-O2" } */

/* { dg-final { scan-assembler "3,\.*3,\.*4" } }

/* Ensure that indexed address are output with base address in rA position
   and index in rB position.  */

char
do_one (char *base, unsigned long offset)
{
  return base[offset];
}

