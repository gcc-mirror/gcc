/* PR tree-optimization/87859 */
/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-store-merging-details" } */
/* { dg-final { scan-tree-dump "New sequence of \[23] stores to replace old one of 14 stores" "store-merging" { target i?86-*-* x86_64-*-* } } } */
/* { dg-final { scan-tree-dump "New sequence of 1 stores to replace old one of 6 stores" "store-merging" { target i?86-*-* x86_64-*-* } } } */

struct S {
  union F {
    struct T {
#define A(n) unsigned n:1;
#define B(n) A(n##0) A(n##1) A(n##2) A(n##3) A(n##4) \
	     A(n##5) A(n##6) A(n##7) A(n##8) A(n##9)
      B(f) B(f1) B(f2) B(f3) B(f4) B(f5)
      A(f60) A(f61) A(f62) A(f63) A(f64) A(f65) A(f66)
    } q;
    unsigned int i[3];
  } f;
};

struct S s = {
  .f.q.f0 = 1, .f.q.f1 = 1, .f.q.f2 = 1, .f.q.f5 = 1, .f.q.f6 = 1,
  .f.q.f7 = 1, .f.q.f8 = 1, .f.q.f9 = 1, .f.q.f13 = 1, .f.q.f14 = 1,
  .f.q.f15 = 1, .f.q.f16 = 1, .f.q.f17 = 1, .f.q.f19 = 1, .f.q.f21 = 1,
  .f.q.f66 = 1
};

__attribute__((noipa)) void
bar (unsigned *x)
{
  if (x[0] != s.f.i[0] || x[1] != s.f.i[1] || x[2] != s.f.i[2])
    __builtin_abort ();
}

__attribute__((noipa)) void
foo (unsigned char *state, unsigned char c)
{
  struct S i;
  i.f.i[0] = 0;
  i.f.i[1] = 0;
  i.f.i[2] = 0;
  i.f.q.f7 = 1;
  i.f.q.f2 = 1;
  i.f.q.f21 = 1;
  i.f.q.f19 = 1;
  i.f.q.f14 = 1;
  i.f.q.f5 = 1;
  i.f.q.f0 = 1;
  i.f.q.f15 = 1;
  i.f.q.f16 = 1;
  i.f.q.f6 = 1;
  i.f.q.f9 = 1;
  i.f.q.f17 = c;
  i.f.q.f1 = 1;
  i.f.q.f8 = 1;
  i.f.q.f13 = 1;
  i.f.q.f66 = 1;
  if (*state)
    {
      i.f.q.f37 = 1;
      i.f.q.f38 = 1;
      i.f.q.f39 = 1;
      i.f.q.f40 = 1;
      i.f.q.f41 = 1;
      i.f.q.f36 = 1;
    }
  bar (i.f.i);
}

int
main ()
{
  unsigned char z = 0;
  foo (&z, 1);
  return 0;
}
