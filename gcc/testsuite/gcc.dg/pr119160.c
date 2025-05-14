/* { dg-do run } */
/* { dg-options "-O2 -finstrument-functions-once -favoid-store-forwarding -fnon-call-exceptions -fschedule-insns -Wno-psabi" } */
/* { dg-additional-options "-mgeneral-regs-only" { target { x86_64-*-* i?86-*-* arm*-*-* aarch64*-*-* } } } */

typedef __attribute__((__vector_size__ (32))) int V;

void
foo (V v, V, V, V *r)
{
  V u = (V){} + v[0];
  *r = u;
}

__attribute__((__noipa__)) void
bar(int x)
{
 if (x != 2) __builtin_abort();
}

int
main ()
{
  V x;
  foo ((V){ 2, 3 }, (V){ }, (V){ }, &x);
  for (unsigned i = 0; i < sizeof(x)/sizeof(x[0]); i++)
    bar(x[i]);
}