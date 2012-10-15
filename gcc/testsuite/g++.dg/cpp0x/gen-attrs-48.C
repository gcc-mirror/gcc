// { dg-do compile { target c++11 } }

typedef char layout_type;

struct A {
  layout_type member alignas (double);
};

static_assert (alignof (A) == alignof (double),
	       "alignment of struct A must be alignof (double)");

struct alignas (alignof (long double)) B {
  layout_type member;
};

static_assert (alignof (B) == alignof (long double),
	       "alignment of struct A must be alignof (double double)");
