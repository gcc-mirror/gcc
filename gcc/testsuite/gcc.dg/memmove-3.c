/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "memmove" 3 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */

char a[40];
struct A { char a[30]; };

void
foo (struct A *p, char *q, char *r)
{
  char b[10];
  __builtin_memmove (&a[1], &a[19], 20);
  __builtin_memmove (&p->a[1], &p->a[9], 10);
  __builtin_memmove (q, r, 9);
}
