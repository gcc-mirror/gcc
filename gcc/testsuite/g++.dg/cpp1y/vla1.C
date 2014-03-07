// { dg-do compile { target c++1y } }

#include <typeinfo>

void f(int n)
{
  int a[n];
  int aa[n][n];			// { dg-error "" }
  &a;				// { dg-error "" }
  sizeof a;			// { dg-error "" }
  typeid(a);			// { dg-error "" }
  decltype(a) a2;		// { dg-error "" }
  typedef int at[n];		// { dg-error "" }
  int (*p)[n];			// { dg-error "" }
  int (&r)[n] = a;		// { dg-error "" }
  struct A
  {
    int a[n];			// { dg-error "" }
  };
}

template <class T>
void g(int n)
{
  int a[n];
  int aa[n][n];			// { dg-error "" }
  &a;				// { dg-error "" }
  sizeof a;			// { dg-error "" }
  typeid(a);			// { dg-error "" }
  decltype(a) a2;		// { dg-error "" }
  typedef int at[n];		// { dg-error "" }
  int (*p)[n];			// { dg-error "" }
  int (&r)[n] = a;		// { dg-error "" }
  struct A
  {
    int a[n];			// { dg-error "" }
  };
}

template void g<int>(int);
