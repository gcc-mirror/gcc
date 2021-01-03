/* PR rtl-optimization/97421 */
/* { dg-additional-options "-fmodulo-sched" } */

int a, b, d, e;
int *volatile c = &a;

__attribute__((noinline))
void f(void)
{
  for (int g = 2; g >= 0; g--) {
    d = 0;
    for (b = 0; b <= 2; b++)
      ;
    e = *c;
  }
}

int main(void)
{
  f();
  if (b != 3)
    __builtin_abort();
}
