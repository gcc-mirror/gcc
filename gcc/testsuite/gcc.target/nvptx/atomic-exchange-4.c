/* Test the atomic exchange expansion, execution.  */

/* { dg-do run } */
/* { dg-options "-Wno-long-long" } */

/* We're trying to generate this type of store/exchange/load sequence:
     st.global.u32   [g32], %r60;
     atom.global.exch.b32    %r22, [g32], 2;
     ld.global.u32   %r23, [g32];
   with no insns inbetween.

   We compile this at -O0, to keep the compiler from optimizing out the
   "p = (P)" assignment.  If the assignment is optimized out we don't test
   the generic case, iow we generate for instance atom.global.exch.b32 instead
   of atom.exch.b32.

   Compiling at -O0 however does introduce loads and stores in the
   store/exchange/load sequence, so we fix that by using the register
   keyword.  */

enum memmodel
{
  MEMMODEL_RELAXED = 0,
};

unsigned int g32;
unsigned long long int g64;

unsigned int s32 __attribute__((shared));
unsigned long long int s64 __attribute__((shared));

#define TEST(P, V1, V2)						\
  {								\
    register typeof (*(P)) tmp;					\
    register typeof (*(P)) tmp2;				\
    __atomic_store_n ((P), (V1), MEMMODEL_RELAXED);		\
    tmp = __atomic_exchange_n ((P), (V2), MEMMODEL_RELAXED);	\
    tmp2 = __atomic_load_n ((P), MEMMODEL_RELAXED);		\
    if (tmp != (V1) || tmp2 != (V2))				\
      __builtin_abort ();					\
  }

#define TEST2(P, V1, V2)					\
  {								\
    register typeof (*(P)) tmp;					\
    register typeof (*(P)) tmp2;				\
    *(P) = (V1);						\
    tmp = __atomic_exchange_n ((P), (V2), MEMMODEL_RELAXED);	\
    tmp2 = *(P);						\
    if (tmp != (V1) || tmp2 != (V2))				\
      __builtin_abort ();					\
  }

#define TESTS(P)				\
  {						\
    TEST ((P), 1, 2);				\
    TEST2 ((P), 3, 4);				\
    {						\
      register typeof (*(P)) * p = (P);		\
      TEST (p, 1, 2);				\
      TEST2 (p, 3, 4);				\
    }						\
  }

int
main ()
{
  TESTS (&g32);
  TESTS (&g64);
  TESTS (&s32);
  TESTS (&s64);

  return 0;
}
