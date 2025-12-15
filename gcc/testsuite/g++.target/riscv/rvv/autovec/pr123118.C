/* { dg-do compile } */
/* { dg-options "-O3 -march=rv64gcv -mabi=lp64d" } */

long long a;
short c, d;
extern int e[][1][1][1];
extern bool f[][1][4][2];
#include <vector>

void
g ()
{
  for (bool b;;)
    for (signed char h (c); h < 4; h += -4487 - 119)
      {
	e[b][b][b][b] = std::max (std::min ((long long) 3, a), (long long) d);
	f[0][0][h][1] = h;
      }
}
