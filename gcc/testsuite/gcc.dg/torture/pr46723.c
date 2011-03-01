/* { dg-do compile } */

short *m;
void test()
{
  short x = 128;
  unsigned int i;
  for (i = 0; i < 128; ++i, x = (unsigned short)x + 1)
    m[i] = x;
}
