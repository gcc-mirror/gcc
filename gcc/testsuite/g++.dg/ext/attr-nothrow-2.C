/*  PR c++/83871 - wrong code for attribute const and pure on distinct
    template specializations
    Test to verify that attributes nothrow on multiple declarations of
    the same function are merged.
    { dg-do compile }
    { dg-options "-O -fdump-tree-eh" } */

void __attribute__ ((nothrow)) fnothrow ();

void test_nothrow () throw ()
{
  // No exception handling necessary around the call to fnothrow().
  fnothrow ();
}

void __attribute__ ((nothrow)) fnothrow_none ();
void fnothrow_none ();

void test_nothrow_none () throw ()
{
  // No exception handling necessary around the call to fnothrow_none().
  fnothrow_none ();
}

void fnone_nothrow ();
void __attribute__ ((nothrow)) fnone_nothrow ();

void test_none_nothrow () throw ()
{
  // No exception handling necessary around the call to fnone_nothrow().
  fnone_nothrow ();
}

int __attribute__ ((nothrow)) fnothrow_noreturn_none ();
int __attribute__ ((noreturn)) fnothrow_noreturn_none ();
int fnothrow_noreturn_none ();

int test_nothrow_noreturn_none () throw ()
{
  // No exception handling necessary around the call().
  // No -Wreturn-value should be emitted because the function is
  // declared noreturn.
  fnothrow_noreturn_none ();
}

// Verify that no exception handling code was emitted.
// { dg-final { scan-tree-dump-not "eh_dispatch" "eh" } }
// { dg-final { scan-tree-dump-not "resx" "eh" } }
