/* { dg-do run } */
/* { dg-options "-march=x86-64-v3 -O2 -flax-vector-conversions" } */
/* { dg-require-effective-target avx2 } */

#include <x86intrin.h>

#include "avx2-check.h"

#ifndef TEST
#define TEST avx2_test
#endif

typedef short __v2hi __attribute__ ((__vector_size__ (4)));
typedef char __v2qi __attribute__ ((__vector_size__ (2)));
typedef char __v4qi __attribute__ ((__vector_size__ (4)));
typedef char __v8qi __attribute__ ((__vector_size__ (8)));

typedef union
{
  __v2si x;
  int a[2];
} union64i_d;

typedef union
{
  __v2hi x;
  short a[2];
} union32i_w;

typedef union
{
  __v4hi x;
  short a[4];
} union64i_w;

typedef union
{
  __v2qi x;
  char a[2];
} union16i_b;

typedef union
{
  __v4qi x;
  char a[4];
} union32i_b;

typedef union
{
  __v8qi x;
  char a[8];
} union64i_b;

#define CHECK_EXP_LESS128(UNION_TYPE, VALUE_TYPE, FMT)	  \
static int						  \
__attribute__((noinline, unused))			  \
check_##UNION_TYPE (UNION_TYPE u, const VALUE_TYPE * v)	  \
{							  \
  int i;						  \
  int err = 0;						  \
							  \
  for (i = 0; i < ARRAY_SIZE (u.a); i++)		  \
    if (u.a[i] != v[i])					  \
      {							  \
	err++;						  \
	PRINTF ("%i: " FMT " != " FMT "\n",		  \
		i, v[i], u.a[i]);			  \
      }							  \
  return err;						  \
}

CHECK_EXP_LESS128 (union64i_d, int, "%d");
CHECK_EXP_LESS128 (union32i_w, short, "%d");
CHECK_EXP_LESS128 (union64i_w, short, "%d");
CHECK_EXP_LESS128 (union16i_b, char, "%d");
CHECK_EXP_LESS128 (union32i_b, char, "%d");
CHECK_EXP_LESS128 (union64i_b, char, "%d");

#define SUBTEST(INPUT_TYPE, OUTPUT_TYPE, OUTPUT_INNER, INIT_TYPE, CVT_TYPE) \
void do_test##INIT_TYPE##CVT_TYPE ()			  \
{							  \
  INPUT_TYPE s;						  \
  OUTPUT_TYPE r, ref;					  \
  for (int i = 0; i < ARRAY_SIZE (s.a); i++)		  \
    {							  \
      s.a[i] = (i + 23415) * (i + 341);			  \
      ref.a[i] = (OUTPUT_INNER) s.a[i];			  \
    }							  \
  r.x = __builtin_convertvector((INIT_TYPE)s.x, CVT_TYPE); \
							  \
  if (check_##OUTPUT_TYPE (r, ref.a))			  \
    abort ();						  \
  return;						  \
}

SUBTEST(union128i_q, union64i_d, int, __v2di, __v2si);
SUBTEST(union256i_q, union128i_d, int, __v4di, __v4si);
SUBTEST(union128i_q, union32i_w, short, __v2di, __v2hi);
SUBTEST(union256i_q, union64i_w, short, __v4di, __v4hi);
SUBTEST(union64i_d, union32i_w, short, __v2si, __v2hi);
SUBTEST(union128i_d, union64i_w, short, __v4si, __v4hi);
SUBTEST(union256i_d, union128i_w, short, __v8si, __v8hi);
SUBTEST(union128i_q, union16i_b, char, __v2di, __v2qi);
SUBTEST(union256i_q, union32i_b, char, __v4di,__v4qi);
SUBTEST(union64i_d, union16i_b, char, __v2si, __v2qi);
SUBTEST(union128i_d, union32i_b, char, __v4si, __v4qi);
SUBTEST(union256i_d, union64i_b, char, __v8si, __v8qi);
SUBTEST(union32i_w, union16i_b, char, __v2hi, __v2qi);
SUBTEST(union64i_w, union32i_b, char, __v4hi, __v4qi);
SUBTEST(union128i_w, union64i_b, char, __v8hi, __v8qi);
SUBTEST(union256i_w, union128i_b, char, __v16hi, __v16qi);

void TEST (void)
{
  do_test__v2di__v2si ();
  do_test__v2di__v2hi ();
  do_test__v2di__v2qi ();
  do_test__v4di__v4si ();
  do_test__v4di__v4hi ();
  do_test__v4di__v4qi ();
  do_test__v2si__v2hi ();
  do_test__v2si__v2qi ();
  do_test__v4si__v4hi ();
  do_test__v4si__v4qi ();
  do_test__v8si__v8hi ();
  do_test__v8si__v8qi ();
  do_test__v2hi__v2qi ();
  do_test__v4hi__v4qi ();
  do_test__v8hi__v8qi ();
  do_test__v16hi__v16qi ();
}
