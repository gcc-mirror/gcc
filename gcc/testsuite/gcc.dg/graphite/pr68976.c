/* { dg-options "-O2 -floop-nest-optimize" } */

int kw = -1, hv = -1, ju;
int mc[1];
void xx(void)
{
  for (; kw; ++kw)
    for (; hv; ++hv)
      for (ju = 0; ju < 2; ++ju)
        mc[kw+1] = mc[0];
}
