/* PR middle-end/122033 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

void g(int*);
void h();
double t;
void f(int a, int b)
{
  {
    int array0[a];
    {
      int array1[b];
      g(array0);
      g(array1);
    }
    t = __builtin_sin(t);
  }
  h ();
}

/* { dg-final { scan-tree-dump-times "__builtin_stack_save" 2 "optimized"} } */
/* { dg-final { scan-tree-dump-times "__builtin_stack_restore" 2 "optimized"} } */
