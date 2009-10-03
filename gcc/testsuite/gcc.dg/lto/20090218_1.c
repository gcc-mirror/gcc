void bar(void);
void  __attribute__((noinline)) *foo1 (void)
{
  bar();
  return (void *) bar;
}
