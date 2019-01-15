/* PR rtl-optimization/88296 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -march=core2 -fnon-call-exceptions -fschedule-insns -fselective-scheduling -funroll-all-loops -fno-caller-saves -fno-guess-branch-probability -fno-ivopts -fno-rerun-cse-after-loop -fno-split-wide-types -fno-tree-bit-ccp -fno-tree-coalesce-vars --param max-completely-peeled-insns=13" } */

int a, b, c;
void bar (int, int);

void
foo (__int128 x, unsigned int y, char z)
{
  __int128 *d = &x;
  unsigned short int e = 0;
  unsigned char f = 0;
  char g = 1;
  (void) d;
  bar (0, 0);
  while (e < 1)
    {
      __int128 h = 1;
      ++c;
      e = x;
      g /= e;
      if (g != 0)
        {
          int i;
          if (g == 0)
            i = a;
          else
            {
              ++g;
              i = g;
            }
          if (i == 0)
            {
              a -= b + (!!g ? y : 0);
              break;
            }
          for (g = 0; g < 3; ++g)
            {
              unsigned char *j = &f;
              x /= h;
              h /= x;
              for (i = 0; i < 2; ++i)
                {
                  z <<= ((!!g && !!c) ? (b + 1) : (1 / *j));
                  y = ~e;
                  g &= y;
                  x |= 1;
                  b *= i;
                  ++c;
                }
              f /= b > 1;
              e /= f;
              x += b << 32;	/* { dg-warning "left shift count" } */
            }
          if (g << g == 0)
            {
              e *= b + 1;
              a = 32;
              f += (e << a) + (y && 1);
              x = z;
              c += x / 3;
            }
        }
    }
}
