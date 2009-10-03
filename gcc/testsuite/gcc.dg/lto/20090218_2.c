void bar(void);
void  __attribute__((noinline))  *foo2 (void)
{
  bar();
  return (void *) bar;
}
