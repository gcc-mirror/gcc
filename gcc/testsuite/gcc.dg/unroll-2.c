/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-cunrolli-details=stderr -fno-peel-loops -fno-tree-vrp  -fdisable-tree-cunroll -fenable-tree-cunrolli" } */

/* Blank lines can occur in the output of
   -fdump-tree-cunrolli-details=stderr.  */
/* { dg-allow-blank-lines-in-output 1 } */

unsigned a[100], b[100];
inline void bar()
{
 a[10] = b[10];
}

int foo(void)
{
  int i;
  bar();
  for (i = 0; i < 2; i++) /* { dg-optimized "loop with 2 iterations completely unrolled" } */
  {
     a[i]= b[i] + 1;
  }
  return 1;
}

int foo2(void)
{
  int i;
  for (i = 0; i < 2; i++) /* { dg-optimized "loop with 2 iterations completely unrolled" } */
  {
     a[i]= b[i] + 1;
  }
  return 1;
}
/* { dg-prune-output ".*" } */
