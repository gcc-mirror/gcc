/* { dg-do assemble } */
/* { dg-options "-O2 -save-temps" } */

signed short s;
signed char c;

void
foo (void)
{
  s = c;
}

/* { dg-final { scan-assembler "(?n)cvt\\.s16\\.s8.*%r" } } */
