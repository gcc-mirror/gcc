/* { dg-do compile } */
/* PR tree-optimization/126815 */
int f (int) __attribute__((returns_twice));
void sink1 (int) __attribute__((leaf));
void sink (int);

void h (int a, int b, int c)
{
  int t;
  if (c){
    sink (1);
    t = f (a);
  }
  else
    t = b;
  if (t == 42)
    __builtin_unreachable ();
  sink1 (a);
}
