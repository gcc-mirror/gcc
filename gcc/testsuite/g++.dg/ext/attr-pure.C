/*  PR c++/83871 - wrong code for attribute const and pure on distinct
    template specializations
    { dg-do compile }
    { dg-options "-O -Wall -fdump-tree-optimized" } */

int __attribute__ ((pure)) fpure_none ();
int fpure_none ();

void func_pure_none_failed ();

void func_pure_none ()
{
  int i0 = fpure_none ();
  int i1 = fpure_none ();
  if (i0 != i1)
    func_pure_none_failed ();

  // { dg-final { scan-tree-dump-not "func_pure_none_failed" "optimized" } }
}


int fnone_pure ();
int __attribute__ ((pure)) fnone_pure ();

void func_none_pure_failed ();

void func_none_pure ()
{
  int i0 = fnone_pure ();
  int i1 = fnone_pure ();
  if (i0 != i1)
    func_none_pure_failed ();

  // { dg-final { scan-tree-dump-not "func_none_pure_failed" "optimized" } }
}


template <class T>
int __attribute__ ((pure)) fpure_none (T);

template <class T>
int fpure_none (T);

void templ_pure_none_failed ();

void template_pure_none ()
{
  int i0 = fpure_none<int> (0);
  int i1 = fpure_none<int> (0);
  if (i0 != i1)
    templ_pure_none_failed ();

  // { dg-final { scan-tree-dump-not "templ_pure_none_failed" "optimized" } }
}


template <class T>
int fnone_const (T);

template <class T>
int __attribute__ ((const)) fnone_const (T);

void templ_none_const_failed ();

void test_fnone_const ()
{
  int i0 = fnone_const<int> (0);
  int i1 = fnone_const<int> (0);
  if (i0 != i1)
    templ_none_const_failed ();

  // { dg-final { scan-tree-dump-not "templ_none_const_failed" "optimized" } }
}
