/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

void
test_1 (void)
{
  static unsigned int bm[16];
  int j;
  for (j = 0; j < 16; j++)
    bm[j] <<= 8;
}

void
test_2 (int a)
{
  static unsigned int bm[16];
  int j;
  for (j = 0; j < 16; j++)
    bm[j] <<= a;
}

void
test_3 (void)
{
 static unsigned bm[16];
 int am[16];
 int j;
 for (j = 0; j < 16;j++)
   bm[j] <<= am[j];
}
