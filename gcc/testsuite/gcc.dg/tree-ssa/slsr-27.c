/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-dom2" } */

struct x
{
  int a[16];
  int b[16];
  int c[16];
};

extern void foo (int, int, int);

void
f (struct x *p, unsigned int n)
{
  foo (p->a[n], p->c[n], p->b[n]);
}

/* { dg-final { scan-tree-dump-times "\\* 4;" 1 "dom2" { target { int32 } } } } */
/* { dg-final { scan-tree-dump-times "\\* 2;" 1 "dom2" { target { int16 } } } } */
/* { dg-final { scan-tree-dump-times "p_\\d\+\\(D\\) \\+ \[^\r\n\]*_\\d\+;" 1 "dom2" } } */
/* { dg-final { scan-tree-dump-times "MEM\\\[\\(struct x \\*\\)\[^\r\n\]*_\\d\+" 3 "dom2" } } */
/* { dg-final { cleanup-tree-dump "dom2" } } */
