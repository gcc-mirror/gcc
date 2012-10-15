// { dg-do compile { target c++11 } }

typedef char layout_type;

template<class> struct A {
  layout_type member alignas (double) alignas (int);
};

// Here, the spec says that A<int> should have the stricter alignment,
// so that would be the alignment of 'double', not 'int'.
static_assert (alignof (A<int>) == alignof (double),
	       "alignment of struct A must be alignof (double)");

template<class> struct alignas (1) alignas (alignof (long double)) B {
  layout_type member;
};

// Similarly, the B<int> should have the stricter alignment, so that would
// so that would be the alignment of 'long double', not '1'.
static_assert (alignof (B<int>) == alignof (long double),
	       "alignment of struct A must be alignof (double double)");

