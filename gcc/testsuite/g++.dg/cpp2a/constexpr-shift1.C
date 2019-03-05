// { dg-do compile { target c++11 } }

constexpr int a = -42 << 0;	// { dg-error "left operand of shift expression '\\(-42 << 0\\)' is negative" "" { target c++17_down } }
constexpr int b = -42 << 1;	// { dg-error "left operand of shift expression '\\(-42 << 1\\)' is negative" "" { target c++17_down } }
constexpr int c = -42 << (__SIZEOF_INT__ * __CHAR_BIT__ - 1);	// { dg-error "left operand of shift expression '\\(-42 << \[0-9]*\\)' is negative" "" { target c++17_down } }
				// { dg-warning "result of '\\(-42 << \[0-9]*\\)' requires \[0-9]* bits to represent, but 'int' only has \[0-9]* bits" "" { target c++17_down } .-1 }
constexpr int d = 42 << (__SIZEOF_INT__ * __CHAR_BIT__ - 1);	// { dg-error "shift expression '\\(42 << \[0-9]*\\)' overflows" "" { target c++17_down } }
				// { dg-warning "result of '\\(42 << \[0-9]*\\)' requires \[0-9]* bits to represent, but 'int' only has \[0-9]* bits" "" { target c++17_down } .-1 }
constexpr int e = 32 << (__SIZEOF_INT__ * __CHAR_BIT__ - 5);	// { dg-error "shift expression '\\(32 << \[0-9]*\\)' overflows" "" { target c++17_down } }
				// { dg-warning "result of '\\(32 << \[0-9]*\\)' requires \[0-9]* bits to represent, but 'int' only has \[0-9]* bits" "" { target c++17_down } .-1 }
constexpr int f = 32 << (__SIZEOF_INT__ * __CHAR_BIT__ - 6);
constexpr int g = -42U << 0;
constexpr int h = -42U << 1;
constexpr int i = -42U << (__SIZEOF_INT__ * __CHAR_BIT__ - 1);
constexpr int j = 42U << (__SIZEOF_INT__ * __CHAR_BIT__ - 1);
constexpr int k = 32U << (__SIZEOF_INT__ * __CHAR_BIT__ - 5);
constexpr int l = 32U << (__SIZEOF_INT__ * __CHAR_BIT__ - 6);
#if __cplusplus > 201703L
static_assert (a == g);
static_assert (b == h);
static_assert (c == i);
static_assert (d == j);
static_assert (e == k);
static_assert (f == l);
#endif
