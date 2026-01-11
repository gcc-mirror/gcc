/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized-raw" } */
/* PR tree-optimization/122845 */

struct s1
{
  signed f1:3;
};
struct s1 p;
void f(void)
{
 p.f1 ^= -47;
 p.f1 ^= -47;
}

/* There should be no gimple stmts left except for return.
   That is the ^ has been optimized away.   */
/* { dg-final { scan-tree-dump-not "gimple_assign <" "optimized"  } } */
