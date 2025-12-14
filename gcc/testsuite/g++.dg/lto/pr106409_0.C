/* PR tree-optimization/106409 */
/* { dg-lto-do link } */
/* { dg-lto-options { { -flto -W -Wall -O2 -fno-exceptions } { -flto -W -Wall -O2 -std=c++98 } { -flto -W -Wall -O2 -std=gnu++20 } } } */
struct bb
{
  int t;
  int t1;
  int t2;
  int t3;
};

[[gnu::noipa]]
void *f(unsigned long paramCount)
{
    if (paramCount == 0)
      return 0;
    return new bb[paramCount]();
}

int main(void)
{
  f(100);
}
