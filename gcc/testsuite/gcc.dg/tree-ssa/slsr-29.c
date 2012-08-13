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
  if (n > 3)
    {
      foo (p->a[n], p->c[n], p->b[n]);
      if (n > 12)
	foo (p->b[n], p->a[n], p->c[n]);
    }
}

/* { dg-final { scan-tree-dump-times "\\* 4;" 1 "dom2" } } */
/* { dg-final { scan-tree-dump-times "p_\\d\+\\(D\\) \\+ \[^\r\n\]*_\\d\+" 1 "dom2" } } */
/* { dg-final { scan-tree-dump-times "MEM\\\[\\(struct x \\*\\)\[^\r\n\]*_\\d\+" 9 "dom2" } } */
/* { dg-final { cleanup-tree-dump "dom2" } } */
