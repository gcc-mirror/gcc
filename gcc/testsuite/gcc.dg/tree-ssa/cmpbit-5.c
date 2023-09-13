/* PR tree-optimization/106164 */
/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-forwprop1-details" } */


int f(int a)
{
  int c = a != 2;
  int d = a >= 2;
  return c & d;
}
int g(int b)
{
  int c = b != -1;
  int d = b <= -1;
  return c & d;
}


int g_(int e)
{
  int c = e != -2;
  int d = e <= -2;
  return c & d;
}

int f1(int x)
{
  int c = x == 2;
  int d = x <= 1;
  return c | d;
}
int g1(int y)
{
  int c = y == -1;
  int d = y > -1;
  return c | d;
}
int g1_(int z)
{
  int c = z == -2;
  int d = z >= -1;
  return c | d;
}

/* { dg-final { scan-tree-dump "gimple_simplified to _\[0-9\]+ = a_\[0-9\]+.D. > 2" "forwprop1" } }  */
/* { dg-final { scan-tree-dump "gimple_simplified to _\[0-9\]+ = b_\[0-9\]+.D. < -1" "forwprop1" } }  */
/* { dg-final { scan-tree-dump "gimple_simplified to _\[0-9\]+ = e_\[0-9\]+.D. < -2" "forwprop1" } }  */
/* { dg-final { scan-tree-dump "gimple_simplified to _\[0-9\]+ = x_\[0-9\]+.D. <= 2" "forwprop1" } }  */
/* { dg-final { scan-tree-dump "gimple_simplified to _\[0-9\]+ = y_\[0-9\]+.D. >= -1" "forwprop1" } }  */
/* { dg-final { scan-tree-dump "gimple_simplified to _\[0-9\]+ = z_\[0-9\]+.D. >= -2" "forwprop1" } }  */
