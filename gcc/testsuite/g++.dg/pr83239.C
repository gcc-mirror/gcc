// PR tree-optimization/83239 - False positive from -Wstringop-overflow
// on simple std::vector code
// { dg-do compile }
// { dg-options "-O3 -finline-limit=500 -Wall -fdump-tree-optimized"  }
// { dg-skip-if "requires hosted libstdc++ for vector" { ! hostedlib } }

#include <vector>

// Verify no warnings are issued.

template <class T>
void test_loop ()
{
  std::vector<T> a;

  int num = 2;

  while (num > 0)
    {
      const typename std::vector<T>::size_type sz = a.size ();

      if (sz < 3)
	a.assign (1, 0);
      else
	a.resize (sz - 2);

      --num;
    }
}

// Verify no warnings are issued here either.

template <class T>
void test_if (std::vector<T> &a, int num)
{
  if (num > 0)
    {
      const typename std::vector<T>::size_type sz = a.size ();

      if (sz < 3)
	a.assign (1, 0);
      else
	a.resize (sz - 2);
    }
}

// Instantiate each function on a different type to force both
// to be fully inlined.  Instantiating both on the same type
// causes the inlining heuristics to outline _M_default_append
// which, in turn, masks the warning.
template void test_loop<int>();
template void test_if<long>(std::vector<long>&, int);

// Verify that std::vector<T>::_M_default_append() has been inlined
// (the absence of warnings depends on it).
// { dg-final { scan-tree-dump-not "_ZNSt6vectorIiSaIiEE17_M_default_appendEm"  optimized } }
// { dg-final { scan-tree-dump-not "_ZNSt6vectorIPvSaIS0_EE17_M_default_appendEm" optimized } }
