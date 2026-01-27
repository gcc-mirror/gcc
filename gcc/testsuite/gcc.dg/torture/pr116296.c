/* { dg-do compile } */
/* { dg-require-effective-target int32plus } */

short g(unsigned long h, int i[11][11], short c)
{
  for (unsigned long j = 0; j < h; j -= 0x105f000000000000ull)
    c = c > i[j][j];
  return c;
}
