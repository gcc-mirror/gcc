/* Make sure that gcc understands that an in/out operand is a use as well
   as a def.  */

/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */

void f()
{
  int i = 42;
  int j = 63;
  
  asm ("": "=m"(i), "+r"(j) : "m"(i));
}

/* { dg-final { scan-tree-dump-times "42" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "63" 1 "optimized" } } */
