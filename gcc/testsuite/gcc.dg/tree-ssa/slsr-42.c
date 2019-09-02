/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-slsr-details" } */

struct x
{
  int a[16];
  int b[16];
};

void
set (struct x *p, unsigned int n, int i)
{
  p->a[n] = i;
  p->b[n] = i;
}

/* { dg-final { scan-tree-dump-not "Replacing reference: " "slsr"  { target i?86-*-* x86_64-*-* } } } */
