#include <altivec.h>

/* Test various ways of creating vectors with 2 double words and accessing the
   elements.  This include files supports:

	 testing double
	 testing long on 64-bit systems
	 testing long long on 32-bit systems.

   The endian support is:

	big endian
	little endian.  */

#ifdef DEBUG
#include <stdio.h>
#define DEBUG0(STR)		fputs (STR, stdout)
#define DEBUG2(STR,A,B)		printf (STR, A, B)

static int errors = 0;

#else
#include <stdlib.h>
#define DEBUG0(STR)
#define DEBUG2(STR,A,B)
#endif

#if defined(DO_DOUBLE)
#define TYPE	double
#define STYPE	"double"
#define ZERO	0.0
#define ONE	1.0
#define TWO	2.0
#define THREE	3.0
#define FOUR	4.0
#define FIVE	5.0
#define SIX	6.0
#define FMT	"g"

#elif defined(_ARCH_PPC64)
#define TYPE	long
#define STYPE	"long"
#define ZERO	0L
#define ONE	1L
#define TWO	2L
#define THREE	3L
#define FOUR	4L
#define FIVE	5L
#define SIX	6L
#define FMT	"ld"

#else
#define TYPE	long long
#define STYPE	"long long"
#define ZERO	0LL
#define ONE	1LL
#define TWO	2LL
#define THREE	3LL
#define FOUR	4LL
#define FIVE	5LL
#define SIX	6LL
#define FMT	"lld"
#endif

/* Macros to order the left/right values correctly.  */

#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
#define INIT_ORDER(A, B)	(TYPE) A, (TYPE) B
#define ELEMENT_ORDER(A, B)	(TYPE) A, (TYPE) B
#define ENDIAN			"-mbig"
#else
#define INIT_ORDER(A, B)	(TYPE) B, (TYPE) A
#define ELEMENT_ORDER(A, B)	(TYPE) B, (TYPE) A
#define ENDIAN			"-mlittle"
#endif

static volatile TYPE		five	= FIVE;
static volatile TYPE		six	= SIX;
static volatile vector TYPE	s_v12 = { ONE,   TWO };
static volatile vector TYPE	g_v34 = { THREE, FOUR };


__attribute__((__noinline__))
static void
vector_check (vector TYPE v, TYPE expect_hi, TYPE expect_lo)
{
  TYPE actual_hi, actual_lo;
#ifdef DEBUG
  const char *pass_fail;
#endif

  __asm__ ("xxlor %x0,%x1,%x1"		: "=&wa" (actual_hi) : "wa" (v));
  __asm__ ("xxpermdi %x0,%x1,%x1,3"	: "=&wa" (actual_lo) : "wa" (v));

#ifdef DEBUG
  if ((actual_hi == expect_hi) && (actual_lo == expect_lo))
    pass_fail = ", pass";
  else
    {
      pass_fail = ", fail";
      errors++;
    }

  printf ("Expected %" FMT ", %" FMT ", got %" FMT ", %" FMT "%s\n",
	  expect_hi, expect_lo,
	  actual_hi, actual_lo,
	  pass_fail);
#else
  if ((actual_hi != expect_hi) || (actual_lo != expect_lo))
    abort ();
#endif
}

__attribute__((__noinline__))
static vector TYPE
combine (TYPE op0, TYPE op1)
{
  return (vector TYPE) { op0, op1 };
}

__attribute__((__noinline__))
static vector TYPE
combine_insert (TYPE op0, TYPE op1)
{
  vector TYPE ret = (vector TYPE) { ZERO, ZERO };
  ret = vec_insert (op0, ret, 0);
  ret = vec_insert (op1, ret, 1);
  return ret;
}

__attribute__((__noinline__))
static vector TYPE
concat_extract_00 (vector TYPE a, vector TYPE b)
{
  return (vector TYPE) { vec_extract (a, 0), vec_extract (b, 0) };
}

__attribute__((__noinline__))
static vector TYPE
concat_extract_01 (vector TYPE a, vector TYPE b)
{
  return (vector TYPE) { vec_extract (a, 0), vec_extract (b, 1) };
}

__attribute__((__noinline__))
static vector TYPE
concat_extract_10 (vector TYPE a, vector TYPE b)
{
  return (vector TYPE) { vec_extract (a, 1), vec_extract (b, 0) };
}

__attribute__((__noinline__))
static vector TYPE
concat_extract_11 (vector TYPE a, vector TYPE b)
{
  return (vector TYPE) { vec_extract (a, 1), vec_extract (b, 1) };
}

__attribute__((__noinline__))
static vector TYPE
concat_extract2_0s (vector TYPE a, TYPE b)
{
  return (vector TYPE) { vec_extract (a, 0), b };
}

__attribute__((__noinline__))
static vector TYPE
concat_extract2_1s (vector TYPE a, TYPE b)
{
  return (vector TYPE) { vec_extract (a, 1), b };
}

__attribute__((__noinline__))
static vector TYPE
concat_extract2_s0 (TYPE a, vector TYPE b)
{
  return (vector TYPE) { a, vec_extract (b, 0) };
}

__attribute__((__noinline__))
static vector TYPE
concat_extract2_s1 (TYPE a, vector TYPE b)
{
  return (vector TYPE) { a, vec_extract (b, 1) };
}

__attribute__((__noinline__))
static vector TYPE
concat_extract_nn (vector TYPE a, vector TYPE b, size_t i, size_t j)
{
  return (vector TYPE) { vec_extract (a, i), vec_extract (b, j) };
}

__attribute__((__noinline__))
static vector TYPE
array_0 (vector TYPE v, TYPE a)
{
  v[0] = a;
  return v;
}

__attribute__((__noinline__))
static vector TYPE
array_1 (vector TYPE v, TYPE a)
{
  v[1] = a;
  return v;
}

__attribute__((__noinline__))
static vector TYPE
array_01 (vector TYPE v, TYPE a, TYPE b)
{
  v[0] = a;
  v[1] = b;
  return v;
}

__attribute__((__noinline__))
static vector TYPE
array_01b (TYPE a, TYPE b)
{
  vector TYPE v = (vector TYPE) { 0, 0 };
  v[0] = a;
  v[1] = b;
  return v;
}

int
main (void)
{
  vector TYPE a = (vector TYPE) { ONE,   TWO  };
  vector TYPE b = (vector TYPE) { THREE, FOUR };
  size_t i, j;

  vector TYPE z = (vector TYPE) { ZERO,  ZERO };

  DEBUG2 ("Endian: %s, type: %s\n", ENDIAN, STYPE);
  DEBUG0 ("\nStatic/global initialization\n");
  vector_check (s_v12, INIT_ORDER (1, 2));
  vector_check (g_v34, INIT_ORDER (3, 4));

  DEBUG0 ("\nVector via constant runtime intiialization\n");
  vector_check (a, INIT_ORDER (1, 2));
  vector_check (b, INIT_ORDER (3, 4));

  DEBUG0 ("\nCombine scalars using vector initialization\n");
  vector_check (combine (1, 2), INIT_ORDER (1, 2));
  vector_check (combine (3, 4), INIT_ORDER (3, 4));

  DEBUG0 ("\nSetup with vec_insert\n");
  a = combine_insert (1, 2);
  b = combine_insert (3, 4);
  vector_check (a, ELEMENT_ORDER (1, 2));
  vector_check (b, ELEMENT_ORDER (3, 4));

  DEBUG0 ("\nTesting array syntax\n");
  vector_check (array_0   (a, FIVE),      ELEMENT_ORDER (5, 2));
  vector_check (array_1   (b, SIX),       ELEMENT_ORDER (3, 6));
  vector_check (array_01  (z, FIVE, SIX), ELEMENT_ORDER (5, 6));
  vector_check (array_01b (FIVE, SIX),    ELEMENT_ORDER (5, 6));

  vector_check (array_0   (a, five),      ELEMENT_ORDER (5, 2));
  vector_check (array_1   (b, six),       ELEMENT_ORDER (3, 6));
  vector_check (array_01  (z, five, six), ELEMENT_ORDER (5, 6));
  vector_check (array_01b (five, six),    ELEMENT_ORDER (5, 6));

  DEBUG0 ("\nTesting concat and extract\n");
  vector_check (concat_extract_00 (a, b), INIT_ORDER (1, 3));
  vector_check (concat_extract_01 (a, b), INIT_ORDER (1, 4));
  vector_check (concat_extract_10 (a, b), INIT_ORDER (2, 3));
  vector_check (concat_extract_11 (a, b), INIT_ORDER (2, 4));

  DEBUG0 ("\nTesting concat and extract #2\n");
  vector_check (concat_extract2_0s (a, FIVE), INIT_ORDER (1, 5));
  vector_check (concat_extract2_1s (a, FIVE), INIT_ORDER (2, 5));
  vector_check (concat_extract2_s0 (SIX, a),  INIT_ORDER (6, 1));
  vector_check (concat_extract2_s1 (SIX, a),  INIT_ORDER (6, 2));

  DEBUG0 ("\nTesting variable concat and extract\n");
  for (i = 0; i < 2; i++)
    {
      for (j = 0; j < 2; j++)
	{
	  static struct {
	    TYPE hi;
	    TYPE lo;
	  } hilo[2][2] =
	      { { { ONE, THREE }, { ONE, FOUR } },
		{ { TWO, THREE }, { TWO, FOUR } } };

	  vector_check (concat_extract_nn (a, b, i, j),
			INIT_ORDER (hilo[i][j].hi, hilo[i][j].lo));
	}
    }

  DEBUG0 ("\nTesting separate function\n");
  vector_check (combine (vec_extract (a, 0), vec_extract (b, 0)),
		INIT_ORDER (1, 3));

  vector_check (combine (vec_extract (a, 0), vec_extract (b, 1)),
		INIT_ORDER (1, 4));

  vector_check (combine (vec_extract (a, 1), vec_extract (b, 0)),
		INIT_ORDER (2, 3));

  vector_check (combine (vec_extract (a, 1), vec_extract (b, 1)),
		INIT_ORDER (2, 4));

  vector_check (combine_insert (vec_extract (a, 0), vec_extract (b, 0)),
		ELEMENT_ORDER (1, 3));

  vector_check (combine_insert (vec_extract (a, 0), vec_extract (b, 1)),
		ELEMENT_ORDER (1, 4));

  vector_check (combine_insert (vec_extract (a, 1), vec_extract (b, 0)),
		ELEMENT_ORDER (2, 3));

  vector_check (combine_insert (vec_extract (a, 1), vec_extract (b, 1)),
		ELEMENT_ORDER (2, 4));


#if defined(DO_DOUBLE)
  DEBUG0 ("\nTesting explicit 2df concat\n");
  vector_check (__builtin_vsx_concat_2df (FIVE, SIX), INIT_ORDER (5, 6));
  vector_check (__builtin_vsx_concat_2df (five, six), INIT_ORDER (5, 6));

#elif defined(_ARCH_PPC64)
  DEBUG0 ("\nTesting explicit 2di concat\n");
  vector_check (__builtin_vsx_concat_2di (FIVE, SIX), INIT_ORDER (5, 6));
  vector_check (__builtin_vsx_concat_2di (five, six), INIT_ORDER (5, 6));

#else
  DEBUG0 ("\nSkip explicit 2di concat on 32-bit\n");
#endif

#ifdef DEBUG
  if (errors)
    printf ("\n%d error%s were found", errors, (errors == 1) ? "" : "s");
  else
    printf ("\nNo errors were found.\n");

  return errors;

#else
  return 0;
#endif
}
