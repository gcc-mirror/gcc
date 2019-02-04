void f (void)
{
}
__attribute__((__optimize__("O2")))
void g (void f())
{
  f();
}
__attribute__((__optimize__("O2")))
void h (void)
{
  g(f);
}
