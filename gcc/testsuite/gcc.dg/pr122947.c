/* PR rtl-optimization/122947 based on PR 117239 */
/* { dg-do run } */
/* { dg-options "-fno-inline -O2" } */
/* { dg-additional-options "-fschedule-insns -mno-accumulate-outgoing-args" { target x86 } } */

int c = 1;

struct A {
  int e, f, g, h;
  short i;
  int j;
};

void
bar (int x, struct A y)
{
  if (y.j == 1)
    c = 0;
}

/* Simplest pure way to force baz's x.j back to memory.  
   So simple that IPA "inlines" it, so we disable IPA and mark as pure.  */
int __attribute__ ((noipa, pure))
bad (struct A const *x)
{
  return x->j;
}

int
baz (struct A x)
{
  x.j = 0;
  return bad (&x);
}

int
main ()
{
  struct A k = { 0, 0, 0, 0, 0, 1 };
  int d = baz (k);
  bar (0, k);
  if (c + d != 0)
    __builtin_abort ();
  return 0;
}
