/* { dg-do compile } */
/* { dg-options "-O2" } */

unsigned array[4];

unsigned foo(unsigned long x)
{
          return array[(x>>2)&3ul];
}

/* { dg-final { scan-assembler-not "shr\[^\\n\]*2" } } */
/* { dg-final { scan-assembler "and\[^\\n\]*12" } } */
