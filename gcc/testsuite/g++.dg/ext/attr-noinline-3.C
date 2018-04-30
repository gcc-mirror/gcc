/*  PR c++/84294 - attributes on a function template redeclaration silently
    discarded
    { dg-do compile }
    { dg-options "-O -fdump-tree-optimized" } */

template <void test ()>
void test_func ()
{
  test ();
}

int x;

void __attribute__ ((noinline)) func_noinline_none ();
void func_noinline_none () { x = __LINE__; }

template void test_func<func_noinline_none>();
// { dg-final { scan-tree-dump-times "func_noinline_none *\\(\\);" 1 "optimized" } }


void func_none_noinline ();
void  __attribute__ ((noinline)) func_none_noinline () { x = __LINE__; }

template void test_func<func_none_noinline>();
// { dg-final { scan-tree-dump-times "func_none_noinline *\\(\\);" 1 "optimized" } }


template <class>
void __attribute__ ((noinline)) templ_noinline_none () { x = __LINE__; }

template <class>
void templa_noinline_none ();

template void test_func<templ_noinline_none<int> >();
// { dg-final { scan-tree-dump-times "templ_noinline_none<int> *\\(\\);" 1 "optimized" } }


template <class>
void templ_none_noinline ();

template <class>
void  __attribute__ ((noinline)) templ_none_noinline () { x = __LINE__; }

template void test_func<templ_none_noinline<int> >();
// { dg-final { scan-tree-dump-times "templ_none_noinline<int> *\\(\\);" 1 "optimized" } }
