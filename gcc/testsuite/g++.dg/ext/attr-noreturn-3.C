/*  PR c++/84294 - attributes on a function template redeclaration silently
    discarded
    { dg-do compile }
    { dg-options "-O -fdump-tree-optimized" } */

typedef void Func ();

template <Func>
void fail_func ();

template <Func test>
int test_func ()
{
  test ();

  // Should be eliminated.
  fail_func<test> ();

  // Expect no -Wreturn type here despite the absence of a return
  // statement in a non-void function.
}   // { dg-bogus "\\\[-Wreturn-type]" "bug 84621" { xfail *-*-* } }

void __attribute__ ((noreturn)) func_noreturn_none ();
void func_noreturn_none ();

template int test_func<func_noreturn_none>();


void func_none_noreturn ();
void  __attribute__ ((noreturn)) func_none_noreturn ();

template int test_func<func_none_noreturn>();


template <class>
void __attribute__ ((noreturn)) templ_noreturn_none ();

template <class>
void templa_noreturn_none ();

template int test_func<templ_noreturn_none<int> >();


template <class>
void templ_none_noreturn ();

template <class>
void  __attribute__ ((noreturn)) templ_none_noreturn ();

template int test_func<templ_none_noreturn<int> >();


// Verify that calls to fail_func() specializations have been eliminated.
// { dg-final { scan-tree-dump-not "fail_func" "optimized" } }
