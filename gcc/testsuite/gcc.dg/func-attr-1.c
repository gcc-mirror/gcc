/* Test that setting -Os in a function attribute works.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-rtl-bbro" } */

extern void f1 ();
extern int f2 ();

__attribute__ ((__optimize__ ("O2")))
void
f3()
{
}

__attribute__ ((__optimize__ ("-Os", "-falign-functions", "-falign-jumps", "-falign-labels", "-falign-loops", "-fno-inline-functions", "-foptimize-strlen")))
int
f4 () {
  if (f2 () == 0) {
    f1 ();
  }
  return 0;
}

/* { dg-final { scan-rtl-dump-not "Duplicated bb" "bbro" } } */
