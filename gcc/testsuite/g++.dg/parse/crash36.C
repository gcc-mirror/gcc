// PR c++/32567
// { dg-do compile }
// { dg-options "-std=c++98" }

template <typename... T> struct A	// { dg-error "does not include variadic templates" }
{
  static T &t;				// { dg-error "not expanded with|T" }
  static const int i = sizeof (++t);	// { dg-error "was not declared in this scope" }
};

int x[A <int>::i];	// { dg-error "is not an integral constant-expression" }
