/* { dg-do compile } */
/* { dg-options "-O2 -Warray-bounds" } */

int a[10];
int foo1 (int i)
{
  if (i < 0 || i > 9)
    return a[i];  /* { dg-warning "outside array bounds" } */
  return 0;
}
int foo2 (int i)
{
  if (i < 0 || i > 8)
    return a[i];  /* { dg-bogus "outside array bounds" } */
  return 0;
}
int *foo3 (int i)
{
  if (i < 0 || i > 10)
    return &a[i];  /* { dg-warning "outside array bounds" } */
  return (void *)0;
}
int *foo4 (int i)
{
  if (i < 0 || i > 9)
    return &a[i];  /* { dg-bogus "outside array bounds" } */
  return (void *)0;
}
