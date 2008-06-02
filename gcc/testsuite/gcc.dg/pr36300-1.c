/* { dg-do run } */
/* { dg-options "-O2 -fwrapv" } */

extern void abort (void);

#define VALUE ((int)((long long)U1 * (long long)3) + 2)

int main(void)
{
  long long Y, Y2;
#if(__SIZEOF_INT__ >= 4)
  int U1;
#else
  long U1;
#endif
  int t;
  U1 = -2147483647-1;

  Y = ((long long)(VALUE * VALUE) * 3);

  t = VALUE;
  Y2 = ((long long)(t * t) * 3);

  if (Y != Y2)
    abort ();
  return 0;
}
