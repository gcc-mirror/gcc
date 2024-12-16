/* { dg-do run } */
/* { dg-options "-O -favoid-store-forwarding -mno-push-args --param=store-forwarding-max-distance=0 -Wno-psabi" } */

typedef __attribute__((__vector_size__ (64))) unsigned short V;

__attribute__((__noipa__)) V
foo (V v, V)
{
  return v;
}

int main ()
{
  V a = (V){3, 5, 0, 8, 9, 3, 5, 1, 3, 4, 2, 5, 5, 0, 5, 3, 61886};
  V b = (V){6, 80, 15, 2, 2, 1, 1, 3, 5};
  V x = foo (a, b);
  for (unsigned i = 0; i < sizeof(x)/sizeof(x[0]); i++)
    if (x[i] != a[i])
      __builtin_abort();
}