/* { dg-do run } */
/* { dg-options "-O2" } */

extern void abort (void);

#define VALUE (unsigned int)((int)((long long)U1 * (long long)3) + 2)

int main(void)
{
  int U1;
  long long Y, Y2;
  unsigned int t;

  U1 = -2147483647-1;

  Y = ((long long)(int)(VALUE * VALUE) * 3);

  t = VALUE;
  Y2 = ((long long)(int)(t * t) * 3);

  if (Y != Y2)
    abort ();
  return 0;
}
