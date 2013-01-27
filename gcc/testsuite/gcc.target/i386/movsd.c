/* { dg-do compile } */
/* { dg-options "-O3 -msse2 -mfpmath=sse" } */

volatile double y;

void
test ()
{
  int z;

  for (z = 0; z < 1000; z++)
    y = 1.23;
}

/* { dg-final { scan-assembler-not "(fld|fst)" } } */
