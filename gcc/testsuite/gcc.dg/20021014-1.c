/* { dg-do run } */
/* { dg-options "-O2 -p" } */
/* { dg-error "profiler" "No profiler support" { target mmix-*-* } 0 } */
/* Support for -p on solaris2 relies on mcrt1.o which comes with the
   vendor compiler.  We cannot reiably predict the directory where the
   vendor compiler (and thus mcrt1.o) is installed so we can't
   necessarily find mcrt1.o even if we have it.  */
/* { dg-error "mcrt1.o" "Optional vendor profiler support missing" { target *-*-solaris2* } 0 } */
/* Support for -p on irix relies on libprof1.a which doesn't appear to
   exist on any irix6 system currently posting testsuite results.  */
/* { dg-error "libprof1.a" "Profiler support missing" { target mips*-*-irix* } 0 } */

extern void abort (void);
extern void exit (int);

int foo (void)
{
  static int bar (int x)
  {
    return x + 3;
  }
  return bar (1) + bar (2);
}

int main (void)
{
  if (foo () != 9)
    abort ();
  exit (0);
}
