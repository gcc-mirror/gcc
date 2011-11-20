/* { dg-options "-fprofile-arcs -fvisibility-inlines-hidden" } */
/* { dg-require-visibility "" } */

inline void Boo ()
{
}

extern "C" void (*Foo ()) ()
{
  return Boo;
}

/* { dg-final { scan-assembler "\\.hidden\t__gcov___Z3Boov" } } */
/* { dg-final { scan-assembler "__gcov__Foo:" } } */
/* { dg-final { scan-assembler-not "\\.hidden\t__gcov__Foo" } } */
