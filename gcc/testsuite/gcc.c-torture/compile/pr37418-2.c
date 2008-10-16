typedef void ft(int);
volatile ft f;
void f2(ft *p __attribute__((noreturn)))
{
  p = f;
}
