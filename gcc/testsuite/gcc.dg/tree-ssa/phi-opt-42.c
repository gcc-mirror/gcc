/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

/* PR tree-optimization/116890 */

int f(int a, int b, int c)
{
  int x;
  if (c) x = a == 0;
  else x = 0;
  return x;
}


/* The if should have been removed as the the conversion from bool to int should have been factored out.  */
/* { dg-final { scan-tree-dump-not "if" "optimized" }  }*/
/* { dg-final { scan-tree-dump-times "\[^\r\n\]*_\[0-9\]* = a_\[0-9\]*.D. == 0" 1 "optimized"  } } */
/* { dg-final { scan-tree-dump-times "\[^\r\n\]*_\[0-9\]* = c_\[0-9\]*.D. != 0" 1 "optimized"  } } */

