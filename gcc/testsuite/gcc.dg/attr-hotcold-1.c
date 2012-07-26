void f(void)
{
  goto A;
 A: __attribute__((cold))
  goto B;
 B: __attribute__((hot))
  return;
}
