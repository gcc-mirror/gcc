/* { dg-options "-fprofile-arcs -fvisibility=hidden" } */
/* { dg-require-visibility "" } */

struct __attribute__((visibility ("hidden"))) X
{
  void Fink ();
};

void X::Fink ()
{
}

/* { dg-final { scan-assembler "\\.hidden\t__gcov___ZN1X4FinkEv" { target { ! *-*-darwin*  } } } } */
/* { dg-final { scan-assembler "\\.private_extern ___gcov___ZN1X4FinkEv" { target *-*-darwin* } } } */
