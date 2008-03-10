/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

class bar
{
public:
  static const int conststaticvariable;
};


int f(void)
{
  return bar::conststaticvariable;
}

/* There should be a reference to conststaticvariable since it may
   be overriden at link time.  */
/* { dg-final { scan-tree-dump-times "conststaticvariable" 1 "optimized"} } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
