/* { dg-do compile } */
/* { dg-options "-O2 -Warray-bounds" } */

int a[10];
int *foo1 (int i)
{
  return &a[10]; /* { dg-bogus "above array bounds" } */
}
int *foo2 (int i)
{
  return &a[11]; /* { dg-warning "above array bounds" } */
}
int foo3 (int i)
{
  return a[9]; /* { dg-bogus "above array bounds" } */
}
int foo4 (int i)
{
  return a[10]; /* { dg-warning "above array bounds" } */
}
