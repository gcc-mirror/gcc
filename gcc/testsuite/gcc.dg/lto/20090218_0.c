void  __attribute__((noinline))  *foo1(void);
void  __attribute__((noinline))  *foo2(void);

int main(void)
{
  return foo1() != foo2();
}
