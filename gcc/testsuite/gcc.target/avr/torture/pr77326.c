/* { dg-do run } */
/* { dg-options "-Wl,--defsym,test1=0" } */

extern void test1 (void) __attribute__((weak));

__attribute__((noinline,noclone))
static void va_pseudo (int flag, ...)
{
  __asm ("nop":);
}

__attribute__((noinline,noclone))
static void func (void)
{
  va_pseudo (0, 0, 0, 0);

  if (test1)
    __builtin_abort ();
}

int main (void)
{
  func();
  __builtin_exit (0);
  return 0;
}
