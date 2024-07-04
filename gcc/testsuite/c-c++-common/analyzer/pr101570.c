void
test2 (_Complex double f)
{
  __asm__ ("" : "=r" (__real f));
}
