/* { dg-do compile } */
/* { dg-require-effective-target indirect_jumps } */
/* { dg-require-effective-target label_values } */
/* { dg-options "-O2 -fdump-tree-profile_estimate" } */

int a, b;

void c() {
  &&d;
  void *e = &&f, *g = &&h;
f:
  __attribute__((hot)) h : __attribute__((cold)) for (; a;) goto *g;
d:
  for (; b;)
    goto *e;
}

/* { dg-final { scan-tree-dump-times "predicted to even probabilities" 4 "profile_estimate"} } */

