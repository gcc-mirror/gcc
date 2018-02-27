/*  PR c++/83871 - wrong code for attribute const and pure on distinct
    template specializations
    { dg-do compile }
    { dg-options "-O -Wall" } */

int __attribute__ ((const)) fconst_none ();
int fconst_none ();

void test_const_none_failed ();

void func_const_none ()
{
  int i0 = fconst_none ();
  int i1 = fconst_none ();
  if (i0 != i1)
    test_const_none_failed ();

  // { dg-final { scan-tree-dump-not "test_const_none_failed" "optimized" } }
}


int fnone_const ();
int __attribute__ ((const)) fnone_const ();

void test_none_const_failed ();

void func_none_const ()
{
  int i0 = fnone_const ();
  int i1 = fnone_const ();
  if (i0 != i1)
    test_none_const_failed ();

  // { dg-final { scan-tree-dump-not "test_none_const_failed" "optimized" } }
}


template <class T>
int __attribute__ ((const)) fconst_none (T);

template <class T>
int fconst_none (T);

void template_const_none ()
{
  int i0 = fconst_none<int> (0);
  int i1 = fconst_none<int> (0);
  if (i0 != i1)
    test_const_none_failed ();

  // { dg-final { scan-tree-dump-not "test_const_none_failed" "optimized" } }
}


template <class T>
int fnone_const (T);

template <class T>
int __attribute__ ((const)) fnone_const (T);

void test_fnone_const ()
{
  int i0 = fnone_const<int> (0);
  int i1 = fnone_const<int> (0);
  if (i0 != i1)
    test_none_const_failed ();

  // { dg-final { scan-tree-dump-not "test_none_const_failed" "optimized" } }
}
