/* PR tree-optimization/78428.  */
/* { dg-options "-O2" } */
/* { dg-do run { target int32plus } } */

struct S0
{
  int f2;
  int f3:16;
  int f4:18;
} ;

int a = 5;
struct S0 b = { 3, 0, 0 };
static struct S0 global[2] = { { 77, 0, 78 }, { 77, 0, 78 } };

int main ()
{
  volatile struct S0 *j;
  for (; a;)
    {
      __builtin_printf ("", b.f2);
      j = &b;
      *j = global[1];
      a--;
    }
  return 0;
}
