// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin: PR c++/41020

// Avoid the "-ansi -pedantic" option
// { dg-options "" }
// { dg-do compile }
// { dg-final { scan-assembler "call\[\t \]+_Z4forkv" } }

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
