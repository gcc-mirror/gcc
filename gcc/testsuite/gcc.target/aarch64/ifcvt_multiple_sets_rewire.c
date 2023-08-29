/* { dg-do compile } */
/* { dg-options "-O2 -fdump-rtl-ce1" } */

void sink2(int, int);

void cond1(int cond, int x, int y, int z)
{
  if (x)
    {
      x = y + z;
      y = z + x;
    }

  sink2(x, y);
}

/* { dg-final { scan-assembler-times "csel\tw0, w0, w1" 1 } } */
/* { dg-final { scan-assembler-times "csel\tw1, w3, w2" 1 } } */

/* { dg-final { scan-rtl-dump-times "if-conversion succeeded through noce_convert_multiple_sets" 1 "ce1" } } */
