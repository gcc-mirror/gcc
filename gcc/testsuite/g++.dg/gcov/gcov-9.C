/* { dg-options "-fprofile-arcs -fvisibility-inlines-hidden" } */
/* { dg-require-visibility "" } */

inline void Boo ()
{
}

extern "C" void (*Foo ()) ()
{
  return Boo;
}

/* { dg-final { scan-assembler "\\.hidden\t__gcov___Z3Boov" { target { ! *-*-darwin*  } } } } */
/* { dg-final { scan-assembler "\\.private_extern ___gcov___Z3Boov" { target *-*-darwin* } } } */
/* { dg-final { scan-assembler "__gcov__Foo:" } } */
/* { dg-final { scan-assembler-not "\\.hidden\t__gcov__Foo" { target { ! *-*-darwin*  } } } } */
/* { dg-final { scan-assembler-not "\\.private_extern ___gcov__Foo" { target *-*-darwin* } } } */
