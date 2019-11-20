// PR c++/92504
// { dg-do compile { target c++11 } }
// { dg-additional-options "-O2" }

namespace std {
  typedef __SIZE_TYPE__ size_t;
  typedef __PTRDIFF_TYPE__ ptrdiff_t;
}

struct A {
  A ();
  A (const A &);
  A & operator++ ();
  bool operator != (const A &) const;
  std::ptrdiff_t operator - (const A &);
  A & operator += (std::size_t);
  int a;
  A & begin ();
  A & end ();				// { dg-message "declared here" }
};

void
bar ()
{
  A a;
  #pragma omp for
  for (auto b = a; b != a.end; ++b)	// { dg-error "invalid use of non-static member function" }
    ;
}
