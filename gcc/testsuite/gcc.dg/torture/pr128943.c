/* { dg-do run } */
/* { dg-require-effective-target longlong64 } */

/* The guarded signed x + 10 was used to value-number the unguarded
   (T)((unsigned T)x + 10) in a path not dominated by it.  The code
   involving g makes jump threading duplicate the join block.  */

typedef long long T;
typedef unsigned long long UT;

T X = 0x7ffffffffffffff8, g;
int c, n;

T __attribute__((noipa))
f (T x)
{
  if (n)
    g = x ? 0 : 5;
  x &= 0x7fffffffffffffff;
  T r = 0;
  if (c)
    r = x + 10;
  r += (T)((UT)x + 10) / 6;
  if (c && g == -1)
    return 0;
  return r;
}

int
main (void)
{
  if (f (X) != -1537228672809129301LL)
    __builtin_abort ();
  return 0;
}
