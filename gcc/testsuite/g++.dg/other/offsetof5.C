// PR c++/35741
// { dg-do compile }

#include <stddef.h>

struct A
{
  char c;
  int &i;
};

int j = offsetof (A, i);		// { dg-error "offsetof" }

template <typename T>
struct S
{
  T h;
  T &i;
  static const int j = offsetof (S, i);	// { dg-error "offsetof" }
};

int k = S<int>::j;			// { dg-message "required from here" }
