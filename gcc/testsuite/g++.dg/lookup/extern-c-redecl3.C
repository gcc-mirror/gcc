// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin: PR c++/41020
// { dg-do compile }
// { dg-final { scan-assembler-not "call\[\t \]+_Z4forkv" } }
// { dg-final { scan-assembler "call\[\t \]+fork" } }

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

