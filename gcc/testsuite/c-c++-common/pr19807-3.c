/* Some targets can optimize this on RTL.  */
/* { dg-do link { target { x86_64-*-* i?86-*-* } } } */
/* { dg-options "-O -fdump-tree-optimized" } */

extern void link_error(void);
int i;
int main()
{
  int a[4];
  if (&a[1] + i + 1 != &a[i+2])
    link_error();
  return 0;
}

/* { dg-final { scan-tree-dump-not "link_error" "optimized" { xfail *-*-* } } } */
