/* { dg-do run  { target { i?86-*-* x86_64-*-* } } } */
/* { dg-options "-fcilkplus -O2" } */
/* { dg-additional-options "-lcilkrts" { target { i?86-*-* x86_64-*-* } } } */

int noop(int x)
{
  return x;
}

int post_increment(int *x)
{
  return (*x)++;
}

int main(int argc, char *argv[])
{
  int m = 5;
  int n = m;
  int r = _Cilk_spawn noop(post_increment(&n));
  int n2 = n;
  _Cilk_sync;

  if (r != m || n2 != m + 1)
    return 1;
  else
    return 0;
}

