typedef void ft(int);
void f(int args)__attribute__((const));
void f2(ft *p __attribute__((const)))
{
  p = f;
}
