// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin: PR c++/41020
// { dg-options "" }
// { dg-do compile }
// { dg-final { scan-assembler-not "call\[\t \]+\[^\$\]*?_Z4forkv" { target i?86-*-* x86_64-*-* } } }
// { dg-final { scan-assembler "call\[\t \]+_?fork" { target i?86-*-* x86_64-*-* } } }

typedef __typeof (__builtin_fork ()) pid_t;

extern "C" pid_t fork (void);

void
foo ()
{
  extern pid_t fork (void);
  fork ();
}

extern "C"
pid_t
fork (void)
{
  return 0;
}

