/* PR c/83844 */
/* { dg-do compile { target int32plus } } */
/* { dg-options "-O0 -Wall" } */
/* { dg-require-effective-target alloca } */

typedef unsigned long long __u64 __attribute__((aligned(4),warn_if_not_aligned(8)));
void bar (void *, void *, void *);

void
foo (int n)
{
  struct A
  {
    int i1;
    int i2;
    int i3[n];
    __u64 x;	/* { dg-warning "in 'struct A' may not be aligned to 8" } */
  } __attribute__((aligned (8)));
  struct B
  {
    int i1;
    int i2;
    long long i3[n];
    __u64 x;
  } __attribute__((aligned (8)));
  struct C
  {
    int i1;
    int i2;
    int i3[2 * n];
    __u64 x;
  } __attribute__((aligned (8)));
  struct A a;
  struct B b;
  struct C c;
  bar (&a, &b, &c);
}
