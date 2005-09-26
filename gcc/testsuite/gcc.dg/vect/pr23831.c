/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

void foo (void)
{
  static unsigned int bm[16];
  int j;
  for (j = 0; j < 16; j++)
    bm[j] = bm[j] * 8;
}
