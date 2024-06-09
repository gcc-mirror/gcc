/* { dg-do compile } */
/* { dg-additional-options "-O3" } */

extern _Bool a[];

void
foo (short i, int b[][64][1])
{
  for (; i < 64; i += 4)
    a[i] = b[0][i] != 0;
}
