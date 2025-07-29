/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* PR middle-end/109267 */

int f(void)
{
  __builtin_unreachable();
}

/* This unreachable should be changed to be a trap. */

/* { dg-final { scan-tree-dump-times "__builtin_unreachable trap \\\(" 1 "optimized" { target trap }  } } */
/* { dg-final { scan-tree-dump-times "goto <" 1 "optimized" { target { ! trap } } } } */
/* { dg-final { scan-tree-dump-not "__builtin_unreachable \\\(" "optimized"} } */
