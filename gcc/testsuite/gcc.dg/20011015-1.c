/* { dg-do compile } */
/* { dg-options "-O3 -std=gnu99" } */

char foo (char *x)
{
  return *x;
}

void bar (char *x)
{
  void *arr[foo (x)] __attribute__((unused));
}

void baz (char *x)
{
  bar (x);
}
