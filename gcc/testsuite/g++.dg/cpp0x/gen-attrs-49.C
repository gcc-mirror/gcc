// { dg-do compile { target c++11 } }

typedef char layout_type;

template<class> struct A {
  layout_type member alignas (double);
};

static_assert (alignof (A<int>) == alignof (double),
	       "alignment of struct A must be alignof (double)");

template<class> struct alignas (alignof (long double)) B {
  layout_type member;
};

static_assert (alignof (B<int>) == alignof (long double),
	       "alignment of struct A must be alignof (double double)");
