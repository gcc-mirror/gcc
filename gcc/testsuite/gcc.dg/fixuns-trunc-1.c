/* { dg-do run } */
/* { dg-options "-std=c99" } */

unsigned long foo(double d)
{
  return (unsigned long) d;
}

extern void abort(void);

int main(void)
{
  double d;
  unsigned long l;

#ifdef __LP64__
  d = 9223372036854775808.7;
  l = 1LL << 63;

  if (foo(d) != l)
    abort();
#endif

  d = 122485.2;
  l = 122485;

  if (foo(d) != l)
    abort();

  return 0;
}
