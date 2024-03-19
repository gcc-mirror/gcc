/* { dg-do run } */
/* { dg-require-effective-target int128 } */

unsigned char m[] = {5, 79, 79, 79, 79};
__int128 p;
int main()
{
  int g1 = 0;
  p = 0;
  for (int aj = 0; aj < 256; aj++)
   {
      m[0] = -4;
      for (; p >= 0; p -= 1) {
        g1 = m[p];
      }
  }
  if (g1 != 0xfc)
    __builtin_abort();
}
