// Test whether call saved float and branch regs are restored properly
// { dg-do run { target ia64-*-* } }
// { dg-options "-O2" }

extern "C" void abort (void);

char buf[128];

void __attribute__((noinline))
bar (void)
{
  throw 1;
}

void __attribute__((noinline))
foo (void)
{
  bar ();
  bar ();
}

int
main (void)
{
  register double f2 __asm ("f2");
  register double f3 __asm ("f3");
  register double f4 __asm ("f4");
  register double f5 __asm ("f5");
  register double f16 __asm ("f16");
  register double f17 __asm ("f17");
  register void *b1 __asm ("b1");
  register void *b2 __asm ("b2");
  register void *b3 __asm ("b3");
  register void *b4 __asm ("b4");
  register void *b5 __asm ("b5");
  f2 = 12.0; f3 = 13.0; f4 = 14.0; f5 = 15.0; f16 = 16.0; f17 = 17.0;
  b1 = &buf[1]; b2 = &buf[2]; b3 = &buf[3]; b4 = &buf[4]; b5 = &buf[5];
  try
    {
      foo ();
    }
  catch (...) {}
  if (f2 != 12.0 || f3 != 13.0 || f4 != 14.0
      || f5 != 15.0 || f16 != 16.0 || f17 != 17.0)
    abort ();
  if (b1 != &buf[1] || b2 != &buf[2] || b3 != &buf[3]
      || b4 != &buf[4] || b5 != &buf[5])
    abort ();
  return 0;
}
