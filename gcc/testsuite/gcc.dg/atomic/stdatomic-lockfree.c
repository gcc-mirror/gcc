/* Test atomic_is_lock_free.  */
/* { dg-do run } */
/* { dg-options "-std=c11 -pedantic-errors" } */

#include <stdatomic.h>
#include <stdint.h>

extern void abort ();

_Atomic _Bool aba;
atomic_bool abt;
_Atomic char aca;
atomic_char act;
_Atomic __CHAR16_TYPE__ ac16a;
atomic_char16_t ac16t;
_Atomic __CHAR32_TYPE__ ac32a;
atomic_char32_t ac32t;
_Atomic __WCHAR_TYPE__ awca;
atomic_wchar_t awct;
_Atomic short asa;
atomic_short ast;
_Atomic int aia;
atomic_int ait;
_Atomic long ala;
atomic_long alt;
_Atomic long long alla;
atomic_llong allt;
void *_Atomic apa;

#define CHECK_TYPE(MACRO, V1, V2)		\
  do						\
    {						\
      int r1 = MACRO;				\
      int r2 = atomic_is_lock_free (&V1);	\
      int r3 = atomic_is_lock_free (&V2);	\
      if (r1 != 0 && r1 != 1 && r1 != 2)	\
	abort ();				\
      if (r2 != 0 && r2 != 1)			\
	abort ();				\
      if (r3 != 0 && r3 != 1)			\
	abort ();				\
      if (r1 == 2 && r2 != 1)			\
	abort ();				\
      if (r1 == 2 && r3 != 1)			\
	abort ();				\
      if (r1 == 0 && r2 != 0)			\
	abort ();				\
      if (r1 == 0 && r3 != 0)			\
	abort ();				\
    }						\
  while (0)

int
main ()
{
  CHECK_TYPE (ATOMIC_BOOL_LOCK_FREE, aba, abt);
  CHECK_TYPE (ATOMIC_CHAR_LOCK_FREE, aca, act);
  CHECK_TYPE (ATOMIC_CHAR16_T_LOCK_FREE, ac16a, ac16t);
  CHECK_TYPE (ATOMIC_CHAR32_T_LOCK_FREE, ac32a, ac32t);
  CHECK_TYPE (ATOMIC_WCHAR_T_LOCK_FREE, awca, awct);
  CHECK_TYPE (ATOMIC_SHORT_LOCK_FREE, asa, ast);
  CHECK_TYPE (ATOMIC_INT_LOCK_FREE, aia, ait);
  CHECK_TYPE (ATOMIC_LONG_LOCK_FREE, ala, alt);
  CHECK_TYPE (ATOMIC_LLONG_LOCK_FREE, alla, allt);
  CHECK_TYPE (ATOMIC_POINTER_LOCK_FREE, apa, apa);

  return 0;
}
