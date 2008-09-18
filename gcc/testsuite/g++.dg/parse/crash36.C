// PR c++/32567
// { dg-do compile }
// { dg-options "-std=c++98" }

template <typename... T> struct A	// { dg-warning "variadic templates" }
{
  static T &t;				// { dg-error "not expanded with" "not expanded" }
					// { dg-message "T" "T" { target *-*-* } 7 }
  static const int i = sizeof (++t);	// { dg-error "was not declared in this scope" }
};

int x[A <int>::i];	// { dg-error "is not an integral constant-expression" }
