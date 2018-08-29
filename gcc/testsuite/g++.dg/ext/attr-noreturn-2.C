/*  PR c++/83871 - wrong code for attribute const and pure on distinct
    template specializations
    Test to verify that attributes noreturn on multiple declarations of
    the same function are merged.
    { dg-do compile }
    { dg-options "-O -fdump-tree-optimized" } */

int __attribute__ ((noreturn)) fnoreturn ();

void fnoreturn_failed ();

int test_noreturn () throw ()
{
  fnoreturn ();
  fnoreturn_failed ();
  // Verify that the call to fnoreturn_failed() is eliminated.
  // { dg-final { scan-tree-dump-not "fnoreturn_failed" "optimized" } }

  // Expect no -Wreturn-type warning despite the absence of a return
  // statement in a non-void function.
}


int __attribute__ ((noreturn)) fnoreturn_none ();
int fnoreturn_none ();

void fnoreturn_none_failed ();


int test_noreturn_none ()
{
  fnoreturn_none ();
  fnoreturn_none_failed ();
  // { dg-final { scan-tree-dump-not "fnoreturn_none_failed" "optimized" } }
}

int fnone_noreturn ();
int __attribute__ ((noreturn)) fnone_noreturn ();

void fnone_noreturn_failed ();

int test_none_noreturn () throw ()
{
  fnone_noreturn ();
  fnone_noreturn_failed ();
  // { dg-final { scan-tree-dump-not "fnone_noreturn_failed" "optimized" } }
}
