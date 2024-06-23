/* { dg-do compile }
 * { dg-options "-std=c23 -fno-short-enums" } */

#include <limits.h>

// enumerators are all representable in int
enum E { a = 1ULL, b = _Generic(a, int: 2) };
static_assert(_Generic(a, int: 1));
static_assert(_Generic(b, int: 1));
enum E { a = 1ULL, b = _Generic(a, int: 2) };
static_assert(_Generic(a, int: 1));
static_assert(_Generic(b, int: 1));

// enumerators are not representable in int
enum H { c = 1ULL << (UINT_WIDTH + 1), d = 2 };
static_assert(_Generic(c, enum H: 1));
static_assert(_Generic(d, enum H: 1));
enum H { c = 1ULL << (UINT_WIDTH + 1), d = _Generic(c, enum H: 2) };
static_assert(_Generic(c, enum H: 1));
static_assert(_Generic(d, enum H: 1));

// there is an overflow in the first declaration
enum K { e = UINT_MAX, f, g = _Generic(e, unsigned int: 0) + _Generic(f, unsigned long: 1, unsigned long long: 1) };
static_assert(_Generic(e, enum K: 1));
static_assert(_Generic(f, enum K: 1));
static_assert(_Generic(g, enum K: 1));
enum K { e = UINT_MAX, f, g = _Generic(e, enum K: 0) + _Generic(f, enum K: 1) };
static_assert(_Generic(e, enum K: 1));
static_assert(_Generic(f, enum K: 1));
static_assert(_Generic(g, enum K: 1));

// there is an overflow in the first declaration
enum U { k = INT_MAX, l, m = _Generic(k, int: 0) + _Generic(l, long: 1, long long: 1) };
static_assert(_Generic(k, enum U: 1));
static_assert(_Generic(l, enum U: 1));
static_assert(_Generic(m, enum U: 1));
enum U { k = INT_MAX, l, m = _Generic(k, enum U: 0) + _Generic(l, enum U: 1) };
static_assert(_Generic(k, enum U: 1));
static_assert(_Generic(l, enum U: 1));
static_assert(_Generic(m, enum U: 1));

