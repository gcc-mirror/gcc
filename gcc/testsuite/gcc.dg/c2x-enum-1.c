/* Test C2x enumerations with values not representable in int.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors" } */

/* Check a type while defining an enum (via a diagnostic for incompatible
   pointer types if the wrong type was chosen).  */
#define TYPE_CHECK(cst, type)						\
  cst ## _type_check = sizeof (1 ? (type *) 0 : (typeof (cst) *) 0)

/* Test various explicit values not representable in int.  */

enum e1 { e1a = -__LONG_LONG_MAX__ - 1, TYPE_CHECK (e1a, long long),
	  e1b = 0, TYPE_CHECK (e1b, int),
	  e1c = __LONG_LONG_MAX__, TYPE_CHECK (e1c, long long),
	  e1d = 1, TYPE_CHECK (e1d, int) };
extern enum e1 e1v;
extern typeof (e1a) e1v;
extern typeof (e1b) e1v;
extern typeof (e1c) e1v;
extern typeof (e1d) e1v;
static_assert (sizeof (enum e1) >= sizeof (long long));
static_assert (e1a == -__LONG_LONG_MAX__ - 1);
static_assert (e1b == 0);
static_assert (e1c == __LONG_LONG_MAX__);
static_assert (e1d == 1);
static_assert (e1a < 0);
static_assert (e1c > 0);

/* This is a test where values are representable in int.  */
enum e2 { e2a = (long long) -__INT_MAX__ - 1, TYPE_CHECK (e2a, int),
	  e2b = (unsigned int) __INT_MAX__, TYPE_CHECK (e2b, int),
	  e2c = 2, TYPE_CHECK (e2c, int) };
extern int e2v;
extern typeof (e2a) e2v;
extern typeof (e2b) e2v;
extern typeof (e2c) e2v;
static_assert (e2a == -__INT_MAX__ - 1);
static_assert (e2b == __INT_MAX__);
static_assert (e2c == 2);
static_assert (e2a < 0);
static_assert (e2b > 0);

enum e3 { e3a = 0, TYPE_CHECK (e3a, int),
	  e3b = (unsigned int) -1, TYPE_CHECK (e3b, unsigned int) };
extern enum e3 e3v;
extern typeof (e3a) e3v;
extern typeof (e3b) e3v;
static_assert (e3a == 0u);
static_assert (e3b == (unsigned int) -1);
static_assert (e3b > 0);

/* Test handling of overflow and wraparound (choosing a wider type).  */
#if __LONG_LONG_MAX__ > __INT_MAX__
enum e4 { e4a = __INT_MAX__,
	  e4b, e4c, e4d = ((typeof (e4b)) -1) < 0,
	  e4e = (unsigned int) -1,
	  e4f, e4g = ((typeof (e4e)) -1) > 0,
	  TYPE_CHECK (e4a, int), TYPE_CHECK (e4e, unsigned int) };
extern enum e4 e4v;
extern typeof (e4a) e4v;
extern typeof (e4b) e4v;
extern typeof (e4c) e4v;
extern typeof (e4d) e4v;
extern typeof (e4e) e4v;
extern typeof (e4f) e4v;
extern typeof (e4g) e4v;
static_assert (e4a == __INT_MAX__);
static_assert (e4b == (long long) __INT_MAX__ + 1);
static_assert (e4c == (long long) __INT_MAX__ + 2);
static_assert (e4f == (unsigned long long) (unsigned int) -1 + 1);
/* Verify the type chosen on overflow of a signed type while parsing was
   signed.  */
static_assert (e4d == 1);
/* Verify the type chosen on wraparound of an unsigned type while parsing was
   unsigned.  */
static_assert (e4g == 1);
#endif

/* Likewise, for overflow from long to long long.  */
#if __LONG_LONG_MAX__ > __LONG_MAX__
enum e5 { e5a = __LONG_MAX__,
	  e5b, e5c, e5d = ((typeof (e5b)) -1) < 0,
	  e5e = (unsigned long) -1,
	  e5f, e5g = ((typeof (e5e)) -1) > 0,
#if __LONG_MAX__ > __INT_MAX__
	  TYPE_CHECK (e5a, long),
#else
	  TYPE_CHECK (e5a, int),
#endif
	  TYPE_CHECK (e5e, unsigned long) };
extern enum e5 e5v;
extern typeof (e5a) e5v;
extern typeof (e5b) e5v;
extern typeof (e5c) e5v;
extern typeof (e5d) e5v;
extern typeof (e5e) e5v;
extern typeof (e5f) e5v;
extern typeof (e5g) e5v;
static_assert (e5a == __LONG_MAX__);
static_assert (e5b == (long long) __LONG_MAX__ + 1);
static_assert (e5c == (long long) __LONG_MAX__ + 2);
static_assert (e5f == (unsigned long long) (unsigned long) -1 + 1);
/* Verify the type chosen on overflow of a signed type while parsing was
   signed.  */
static_assert (e5d == 1);
/* Verify the type chosen on wraparound of an unsigned type while parsing was
   unsigned.  */
static_assert (e5g == 1);
#endif
