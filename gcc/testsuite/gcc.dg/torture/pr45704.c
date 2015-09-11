/* { dg-do compile } */
/* { dg-options "-fdump-tree-optimized" } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } { "" } } */

struct st {
    int ptr;
};

int foo(struct st *st)
{
  int v = *(volatile int *)&st->ptr;
  return v & 0xff;
}

/* { dg-final { scan-tree-dump-times "={v}" 1 "optimized" } } */
