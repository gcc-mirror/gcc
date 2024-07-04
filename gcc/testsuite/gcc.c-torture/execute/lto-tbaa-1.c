/* { dg-additional-options "-fno-early-inlining -fno-ipa-cp" }  */
struct a {
  float *b;
} *a;
struct b {
  int *b;
} b;
struct c {
  float *b;
} *c;
int d;
void
use_a (struct a *a)
{
}
void
set_b (int **a)
{
  *a=&d;
}
void
use_c (struct c *a)
{
}
__attribute__ ((noinline)) int **retme(int **val)
{
  return val;
}
int e;
struct b b= {&e};
struct b b2;
struct b b3;
int **ptr = &b2.b;
int
main (void)
{
  a= (void *)0;
  b.b=&e;
  ptr =retme ( &b.b);
  set_b (ptr);
  b3=b;
  if (b3.b != &d)
  __builtin_abort ();
  c= (void *)0;
  return 0;
}
