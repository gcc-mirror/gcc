// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin: PR c++/41020

// { dg-options "" }
// { dg-do compile }
// { dg-final { scan-assembler "call\[\t \]+\[^\$\]*?_Z4forkv" { target i?86-*-* x86_64-*-* } } }

class frok
{
  int this_errno;
  friend int fork (void);
};

void
foo ()
{
  fork ();
}
