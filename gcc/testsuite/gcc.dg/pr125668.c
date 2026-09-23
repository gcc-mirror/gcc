/* { dg-do compile } */
/* { dg-options "-O2 -fno-strict-aliasing -fdump-tree-forwprop1-gimple" } */

struct S {
  long x;
  int y;
};

struct S2 {
  struct S bufs[];
};

void foo(struct S2 *p, int i)
{
  struct S *buf = &p->bufs[i];
  if (*((volatile int *)&buf->y) < 0)
    *((volatile int *)&buf->y) = 0;
}

/* { dg-final { scan-tree-dump-times ", 32>" 2 "forwprop1" { target lp64 } } } */
