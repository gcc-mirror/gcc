// Test whether call saved float are restored properly for O32 ABI
// { dg-do run { target { { { mips*-*-linux* } && hard_float } && { ! mips64 } } } }
// { dg-options "-O2" }

void __attribute__((noinline))
bar (void)
{
  throw 1;
}

void __attribute__((noinline))
foo (void)
{
  register double f20 __asm__ ("f20") = 0.0;
  register double f22 __asm__ ("f22") = 0.0;
  register double f24 __asm__ ("f24") = 0.0;
  register double f26 __asm__ ("f26") = 0.0;
  register double f28 __asm__ ("f28") = 0.0;
  register double f30 __asm__ ("f30") = 0.0;
  __asm__ __volatile__("":"+f"(f20),"+f"(f22),"+f"(f24),"+f"(f26),"+f"(f30));
  bar ();
}

int
main (void)
{
  register double f20 __asm__ ("f20") = 12.0;
  register double f22 __asm__ ("f22") = 13.0;
  register double f24 __asm__ ("f24") = 14.0;
  register double f26 __asm__ ("f26") = 15.0;
  register double f28 __asm__ ("f28") = 16.0;
  register double f30 __asm__ ("f30") = 17.0;

  try
    {
      foo ();
    }
  catch (...)
    {
      __asm__ ("":"+f"(f20),"+f"(f22),"+f"(f24),"+f"(f26),"+f"(f30));
    }

  if (f20 != 12.0 || f22 != 13.0 || f24 != 14.0
      || f26 != 15.0 || f28 != 16.0 || f30 != 17.0)
    __builtin_abort ();
  return 0;
}
