/* PR tree-optimization/48837 */
/* { dg-do run } */
/* { dg-options "-O2" } */

void abort (void);

__attribute__((noinline))
int baz(void)
{
  return 1;
}

inline const int *bar(const int *a, const int *b)
{
 return *a ? a : b;
}

int foo(int a, int b)
{
   return a || b ? baz() : foo(*bar(&a, &b), 1) + foo(1, 0);
}

int main(void)
{
 if (foo(0, 0) != 2)
   abort();

 return 0;
}

