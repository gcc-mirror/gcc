/* { dg-additional-options "-std=gnu89" } */

static void
f()
{
  long long a[2];
  int i;
  if (g())
    if (h())
      ;
  *a |= (long long)i << 65 ;
}
