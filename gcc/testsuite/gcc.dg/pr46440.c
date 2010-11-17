/* PR rtl-optimization/46440 */
/* { dg-do compile } */
/* { dg-options "-O -fstack-protector -fno-tree-dominator-opts -fno-tree-fre" } */
/* { dg-require-effective-target fstack_protector } */

int i;

void bar (char *);

void
foo (void)
{
  void *l;
  char c[64];
  bar (c);
  i = 1;
  if (i)
    l = &&l1;
  else
    l = &&l2;
  goto *l;
l2:
  __builtin_abort ();
l1:;
}
