/*  PR c++/84294 - attributes on a function template redeclaration silently
    discarded
    { dg-do compile }
    { dg-options "-O -fdump-tree-eh -fdump-tree-optimized" } */

typedef void Func ();

template <Func>
void fail_func () throw ();

template <Func test>
void test_func () throw ()
{
  try
    {
      test ();
    }
  catch (...)
    {
      // Should be eliminated.
      fail_func<test> ();
    }
}

void __attribute__ ((nothrow)) func_nothrow_none ();
void func_nothrow_none ();

template void test_func<func_nothrow_none>();


void func_none_nothrow ();
void  __attribute__ ((nothrow)) func_none_nothrow ();

template void test_func<func_none_nothrow>();


template <class>
void __attribute__ ((nothrow)) templ_nothrow_none ();

template <class>
void templa_nothrow_none ();

template void test_func<templ_nothrow_none<int> >();


template <class>
void templ_none_nothrow ();

template <class>
void  __attribute__ ((nothrow)) templ_none_nothrow ();

template void test_func<templ_none_nothrow<int> >();


// Verify that no exception handling code was emitted.
// { dg-final { scan-tree-dump-not "eh_dispatch" "eh" } }
// { dg-final { scan-tree-dump-not "resx" "eh" } }

// Verify that calls to fail_func() specializations have been eliminated.
// { dg-final { scan-tree-dump-not "fail_func" "optimized" } }
