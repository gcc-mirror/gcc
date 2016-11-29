/* Tests for _FloatN / _FloatNx types: test max_align_t alignment.
   Before including this file, define WIDTH as the value N; define EXT
   to 1 for _FloatNx and 0 for _FloatN.  */

#define CONCATX(X, Y) X ## Y
#define CONCAT(X, Y) CONCATX (X, Y)
#define CONCAT3(X, Y, Z) CONCAT (CONCAT (X, Y), Z)

#if EXT
# define TYPE CONCAT3 (_Float, WIDTH, x)
#else
# define TYPE CONCAT (_Float, WIDTH)
#endif

#include <stddef.h>

_Static_assert (_Alignof (max_align_t) >= _Alignof (TYPE),
		"max_align_t must be at least as aligned as _Float* types");
