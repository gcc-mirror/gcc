/* { dg-do assemble } */
/* { dg-options "-O2 -save-temps" } */

void
foo (unsigned short u)
{
  volatile unsigned short u2 = u << 5;
}

void
foo2 (short s)
{
  volatile unsigned short s2 = s << 5;
}

void
foo3 (unsigned short u)
{
  volatile unsigned short u2 = u >> 5;
}

void
foo4 (signed short s)
{
  volatile signed short s2 = s >> 5;
}

/* { dg-final { scan-assembler-times "(?n)shl\\.b16.*%r" 2 } } */
/* { dg-final { scan-assembler "(?n)shr\\.u16.*%r" } } */
/* { dg-final { scan-assembler "(?n)shr\\.s16.*%r" } } */
