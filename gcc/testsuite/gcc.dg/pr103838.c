/* PR debug/103838 */
/* { dg-do compile } */
/* { dg-options "-O2 -fcompare-debug" } */

#ifdef __SIZEOF_INT128__
__int128 m;
#else
long long m;
#endif
int n;

__attribute__((noinline)) void
bar (void)
{
  n += !!m;
}

void
foo (void)
{
  int i;

  for (i = 0; i < 2; ++i)
    {
      bar ();
      m /= 3;
    }
}
