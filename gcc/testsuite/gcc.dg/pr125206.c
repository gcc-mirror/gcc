/* { dg-do compile } */
/* { dg-options "-O -fgimple -fdump-tree-forwprop1-gimple" } */

struct S {
  struct S *next;
  int vals[5];
};

int __GIMPLE foo (struct S * s, int idx)
{
  int D_2968;
  int *_1;

  /* In GIMPLE '*s' does not have to be aligned according to its type
     so we have to preserve the alignment of the access when forwarding
     the address.  */
  _1 = &s->vals[idx];
  D_2968 = __MEM <volatile int> ((volatile int *)_1);
  return D_2968;
}

/* { dg-final { scan-tree-dump "__MEM <struct S, 32>" "forwprop1" { target lp64 } } } */
