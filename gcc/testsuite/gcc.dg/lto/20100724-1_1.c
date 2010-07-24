extern __attribute__((__noreturn__)) void baz(void);
void bar(void)
{
  baz();
}
