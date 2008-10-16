typedef void ft(int);
void f(int args)__attribute__((noreturn));
void f2(ft *p __attribute__((noreturn)))
{
  p = f;
}
