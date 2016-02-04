// Definitions of helper macros for tests of flexible array members.

#if __cplusplus < 201102L
#  define _CAT(x, y)  x ## y
#  define CAT(x, y)  _CAT (x, y)

// Generate a struct with a unique name containing a bitfield
// of size that must evaluate to a non-zero value, otherwise
// generate a compiler error.
#  define ASSERT(expr)                                                  \
  struct CAT (FAM_Assert, __LINE__) { unsigned asrt: 0 != (expr); }
#else
// In C++ 11 and beyond, use static_assert.
# define ASSERT(expr) static_assert (expr, #expr)
#endif

// Macro to verify that a flexible array member is allocated
// at the very end of the containing struct.
#define ASSERT_AT_END(T, m)                             \
  ASSERT (__builtin_offsetof (T, m) == sizeof (T))

typedef __SIZE_TYPE__ size_t;
