/* Test stdatomic.h header contents.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

#include <stdatomic.h>

#ifndef ATOMIC_BOOL_LOCK_FREE
# error ATOMIC_BOOL_LOCK_FREE not defined
#endif

#ifndef ATOMIC_CHAR_LOCK_FREE
# error ATOMIC_CHAR_LOCK_FREE not defined
#endif

#ifndef ATOMIC_CHAR16_T_LOCK_FREE
# error ATOMIC_CHAR16_T_LOCK_FREE not defined
#endif

#ifndef ATOMIC_CHAR32_T_LOCK_FREE
# error ATOMIC_CHAR32_T_LOCK_FREE not defined
#endif

#ifndef ATOMIC_WCHAR_T_LOCK_FREE
# error ATOMIC_WCHAR_T_LOCK_FREE not defined
#endif

#ifndef ATOMIC_SHORT_LOCK_FREE
# error ATOMIC_SHORT_LOCK_FREE not defined
#endif

#ifndef ATOMIC_INT_LOCK_FREE
# error ATOMIC_INT_LOCK_FREE not defined
#endif

#ifndef ATOMIC_LONG_LOCK_FREE
# error ATOMIC_LONG_LOCK_FREE not defined
#endif

#ifndef ATOMIC_LLONG_LOCK_FREE
# error ATOMIC_LLONG_LOCK_FREE not defined
#endif

#ifndef ATOMIC_POINTER_LOCK_FREE
# error ATOMIC_POINTER_LOCK_FREE not defined
#endif

memory_order m0 = memory_order_relaxed;
memory_order m1 = memory_order_consume;
memory_order m2 = memory_order_acquire;
memory_order m3 = memory_order_release;
memory_order m4 = memory_order_acq_rel;
memory_order m5 = memory_order_seq_cst;

atomic_flag af = ATOMIC_FLAG_INIT;

struct s { int i[100]; } sv;
void
f (void)
{
  _Atomic struct s sva = ATOMIC_VAR_INIT (sv);
}

#ifndef kill_dependency
# error kill_dependency not defined
#endif

#define CHECK_ATOMIC_TYPEDEF(A, B)				\
  do								\
    {								\
      A v;							\
      char array1[sizeof (A) == sizeof (B) ? 1 : -1];		\
      char array2[_Alignof (A) == _Alignof (B) ? 1 : -1];	\
    }								\
  while (0)

#include <stddef.h>
#include <stdint.h>

void
check_typedefs (void)
{
  CHECK_ATOMIC_TYPEDEF (atomic_bool, _Atomic _Bool);
  CHECK_ATOMIC_TYPEDEF (atomic_char, _Atomic char);
  CHECK_ATOMIC_TYPEDEF (atomic_schar, _Atomic signed char);
  CHECK_ATOMIC_TYPEDEF (atomic_uchar, _Atomic unsigned char);
  CHECK_ATOMIC_TYPEDEF (atomic_short, _Atomic short);
  CHECK_ATOMIC_TYPEDEF (atomic_ushort, _Atomic unsigned short);
  CHECK_ATOMIC_TYPEDEF (atomic_int, _Atomic int);
  CHECK_ATOMIC_TYPEDEF (atomic_uint, _Atomic unsigned int);
  CHECK_ATOMIC_TYPEDEF (atomic_long, _Atomic long);
  CHECK_ATOMIC_TYPEDEF (atomic_ulong, _Atomic unsigned long);
  CHECK_ATOMIC_TYPEDEF (atomic_llong, _Atomic long long);
  CHECK_ATOMIC_TYPEDEF (atomic_ullong, _Atomic unsigned long long);
  CHECK_ATOMIC_TYPEDEF (atomic_char16_t, _Atomic __CHAR16_TYPE__);
  CHECK_ATOMIC_TYPEDEF (atomic_char32_t, _Atomic __CHAR32_TYPE__);
  CHECK_ATOMIC_TYPEDEF (atomic_wchar_t, _Atomic wchar_t);
  CHECK_ATOMIC_TYPEDEF (atomic_int_least8_t, _Atomic int_least8_t);
  CHECK_ATOMIC_TYPEDEF (atomic_uint_least8_t, _Atomic uint_least8_t);
  CHECK_ATOMIC_TYPEDEF (atomic_int_least16_t, _Atomic int_least16_t);
  CHECK_ATOMIC_TYPEDEF (atomic_uint_least16_t, _Atomic uint_least16_t);
  CHECK_ATOMIC_TYPEDEF (atomic_int_least32_t, _Atomic int_least32_t);
  CHECK_ATOMIC_TYPEDEF (atomic_uint_least32_t, _Atomic uint_least32_t);
  CHECK_ATOMIC_TYPEDEF (atomic_int_least64_t, _Atomic int_least64_t);
  CHECK_ATOMIC_TYPEDEF (atomic_uint_least64_t, _Atomic uint_least64_t);
  CHECK_ATOMIC_TYPEDEF (atomic_int_fast8_t, _Atomic int_fast8_t);
  CHECK_ATOMIC_TYPEDEF (atomic_uint_fast8_t, _Atomic uint_fast8_t);
  CHECK_ATOMIC_TYPEDEF (atomic_int_fast16_t, _Atomic int_fast16_t);
  CHECK_ATOMIC_TYPEDEF (atomic_uint_fast16_t, _Atomic uint_fast16_t);
  CHECK_ATOMIC_TYPEDEF (atomic_int_fast32_t, _Atomic int_fast32_t);
  CHECK_ATOMIC_TYPEDEF (atomic_uint_fast32_t, _Atomic uint_fast32_t);
  CHECK_ATOMIC_TYPEDEF (atomic_int_fast64_t, _Atomic int_fast64_t);
  CHECK_ATOMIC_TYPEDEF (atomic_uint_fast64_t, _Atomic uint_fast64_t);
  CHECK_ATOMIC_TYPEDEF (atomic_intptr_t, _Atomic intptr_t);
  CHECK_ATOMIC_TYPEDEF (atomic_uintptr_t, _Atomic uintptr_t);
  CHECK_ATOMIC_TYPEDEF (atomic_size_t, _Atomic size_t);
  CHECK_ATOMIC_TYPEDEF (atomic_ptrdiff_t, _Atomic ptrdiff_t);
  CHECK_ATOMIC_TYPEDEF (atomic_intmax_t, _Atomic intmax_t);
  CHECK_ATOMIC_TYPEDEF (atomic_uintmax_t, _Atomic uintmax_t);
}
