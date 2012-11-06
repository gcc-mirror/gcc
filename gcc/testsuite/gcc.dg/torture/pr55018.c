/* PR tree-optimization/55018 */
/* { dg-do compile } */
/* { dg-options "-fdump-tree-optimized" } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } { "" } } */

void
foo (int x)
{
  unsigned int a = 0;
  int b = 3;
  if (x)
    b = 0;
lab:
  if (x)
    goto lab;
  a++;
  if (b != 2)
    __builtin_printf ("%u", a);
  goto lab;
}

/* { dg-final { scan-tree-dump "printf" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
