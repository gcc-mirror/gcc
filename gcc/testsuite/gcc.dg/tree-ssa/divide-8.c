/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */
/* PR tree-optimization/113301 */
/* We should figure out that 1/(x+1) range is [-1,1]
   and then /2 is always 0. */

void link_error(void);
void func(int x){
    int c=(1/(x+1))/2;
    if (c != 0)
      link_error();
}
/* { dg-final { scan-tree-dump-not "link_error " "optimized" } } */
