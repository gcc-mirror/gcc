#pragma GCC system_header

extern void __assert_fail (const char *expr, const char *file, int line)
  __attribute__ ((__noreturn__));

#define assert(EXPR) \
  do { if (!(EXPR)) __assert_fail (#EXPR, __FILE__, __LINE__); } while (0)
