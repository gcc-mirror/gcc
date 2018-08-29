// Bug c++/84617 - new test cases g++.dg/ext/attr-const.C and
// g++.dg/ext/attr-pure.C fail
// { dg-do compile }
// { dg-options "-O -Wall -fdump-tree-optimized" }

static char a[8];

void* __attribute__ ((malloc))
func_malloc_none (unsigned);

void*
func_alloc_none (unsigned);         // redeclare with no attribute

void func_malloc_none_failed ();

void test_func_malloc_none (void)
{
  void *p = func_malloc_none (1);
  if (!p)
    return;

  if (p == a)                       // must be false
    func_malloc_none_failed ();     // should be eliminated

  // Verify that the call to func_malloc_none_failed() is eliminated.
  // { dg-final { scan-tree-dump-not "func_malloc_none_failed" "optimized" } }
}


void*
func_none_malloc (unsigned);

void*  __attribute__ ((malloc))
func_none_malloc (unsigned);         // redeclare with an attribute

void func_none_malloc_failed ();

void test_func_none_malloc (void)
{
  void *p = func_none_malloc (1);
  if (!p)
    return;

  if (p == a)                       // must be false
    func_none_malloc_failed ();     // should be eliminated

  // Verify that the call to func_none_malloc_failed() is eliminated.
  // { dg-final { scan-tree-dump-not "func_none_malloc_failed" "optimized" } }
}


template <class>
void* __attribute__ ((malloc))
templ_malloc_none (unsigned);

template <class>
void*
templ_malloc_none (unsigned);       // redeclare with no attribute

void templ_malloc_none_failed ();

void test_templ_malloc_none (void)
{
  void *p = templ_malloc_none<void>(1);
  if (!p)
    return;

  if (p == a)                       // must be false
    templ_malloc_none_failed ();    // should be eliminated

  // Verify that the call to templ_malloc_none_failed() is eliminated.
  // { dg-final { scan-tree-dump-not "templ_malloc_none_failed" "optimized" } }
}

template <class>
void*
templ_none_malloc (unsigned);

template <class>
void* __attribute__ ((malloc))
templ_none_malloc (unsigned);       // redeclared with an attribute

void templ_none_malloc_failed ();

void test_templ_none_malloc (void)
{
  void *p = templ_none_malloc<void>(1);
  if (!p)
    return;

  if (p == a)                       // must be false
    templ_none_malloc_failed ();    // should be eliminated

  // Verify that the call to templ_none_malloc_failed() is eliminated.
  // { dg-final { scan-tree-dump-not "templ_none_malloc_failed" "optimized" } }
}
