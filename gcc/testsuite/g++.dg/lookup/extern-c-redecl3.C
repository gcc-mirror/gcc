// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin: PR c++/41020
// { dg-options "" }
// { dg-do compile }
// { dg-final { scan-assembler-not "call\[\t \]+\[^\$\]*?_Z4forkv" { target i?86-*-* x86_64-*-* } } }
// { dg-final { scan-assembler "call\[\t \]+_?fork" { target i?86-*-* x86_64-*-* } } }

extern "C" int fork (void);

void
foo ()
{
  extern int fork (void);
  fork ();
}

extern "C"
int
fork (void)
{
  return 0;
}

