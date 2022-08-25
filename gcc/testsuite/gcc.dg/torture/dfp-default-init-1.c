/* Test that default-initialized DFP values consistently have the least quantum
   exponent.  */
/* { dg-do run } */
/* { dg-require-effective-target dfp } */

extern void exit (int);
extern void abort (void);
void *memset (void *, int, __SIZE_TYPE__);
int memcmp (const void *, const void *, __SIZE_TYPE__);

#ifndef TYPE
#define TYPE _Decimal32
#endif

#ifndef ZEROFP
#define ZEROFP 0e-101DF
#endif

TYPE zero_int = 0;
TYPE zero_fp = ZEROFP;
TYPE default_init;
TYPE empty_init = {};
TYPE zero_bytes;
TYPE x;

struct s { TYPE a, b; };
struct s s_default_init;
struct s s_empty_init = {};
struct s s_first_int = { 0 };
struct s s_both_int = { 0, 0 };
struct s sx;

const TYPE a_default_init[10];
const TYPE a_empty_init[10] = {};
const TYPE a_first_int[10] = { 0 };
const TYPE a_two_int[10] = { 0, 0 };

#define CHECK_ZERO_BYTES(expr)					\
  do								\
    {								\
      if (memcmp (expr, &zero_bytes, sizeof zero_bytes) != 0)	\
	abort ();						\
      TYPE tmp = *expr;						\
      if (memcmp (&tmp, &zero_bytes, sizeof zero_bytes) != 0)	\
	abort ();						\
    }								\
  while (0)

#define CHECK_INT_BYTES(expr)					\
  do								\
    {								\
      if (memcmp (expr, &zero_int, sizeof zero_int) != 0)	\
	abort ();						\
      TYPE tmp = *expr;						\
      if (memcmp (&tmp, &zero_int, sizeof zero_int) != 0)	\
	abort ();						\
    }								\
  while (0)

int
main (void)
{
  memset (&zero_bytes, 0, sizeof zero_bytes);
  if (memcmp (&zero_bytes, &zero_int, sizeof zero_int) == 0)
    abort ();
  CHECK_ZERO_BYTES (&zero_fp);
  CHECK_ZERO_BYTES (&default_init);
  CHECK_ZERO_BYTES (&empty_init);
  CHECK_ZERO_BYTES (&s_default_init.a);
  CHECK_ZERO_BYTES (&s_default_init.b);
  CHECK_ZERO_BYTES (&s_empty_init.a);
  CHECK_ZERO_BYTES (&s_empty_init.b);
  CHECK_INT_BYTES (&s_first_int.a);
  CHECK_ZERO_BYTES (&s_first_int.b);
  CHECK_INT_BYTES (&s_both_int.a);
  CHECK_INT_BYTES (&s_both_int.b);
  CHECK_ZERO_BYTES (&a_default_init[0]);
  CHECK_ZERO_BYTES (&a_default_init[1]);
  CHECK_ZERO_BYTES (&a_default_init[2]);
  CHECK_ZERO_BYTES (&a_default_init[9]);
  CHECK_ZERO_BYTES (&a_empty_init[0]);
  CHECK_ZERO_BYTES (&a_empty_init[1]);
  CHECK_ZERO_BYTES (&a_empty_init[2]);
  CHECK_ZERO_BYTES (&a_empty_init[9]);
  CHECK_INT_BYTES (&a_first_int[0]);
  CHECK_ZERO_BYTES (&a_first_int[1]);
  CHECK_ZERO_BYTES (&a_first_int[2]);
  CHECK_ZERO_BYTES (&a_first_int[9]);
  CHECK_INT_BYTES (&a_two_int[0]);
  CHECK_INT_BYTES (&a_two_int[1]);
  CHECK_ZERO_BYTES (&a_two_int[2]);
  CHECK_ZERO_BYTES (&a_two_int[9]);
  struct s s2 = {};
  CHECK_ZERO_BYTES (&s2.a);
  CHECK_ZERO_BYTES (&s2.b);
  struct s s3 = { 0 };
  CHECK_INT_BYTES (&s3.a);
  CHECK_ZERO_BYTES (&s3.b);
  struct s s4 = { 0, 0 };
  CHECK_INT_BYTES (&s4.a);
  CHECK_INT_BYTES (&s4.b);
  struct s s5 = { 0 };
  sx = s5;
  CHECK_INT_BYTES (&sx.a);
  CHECK_ZERO_BYTES (&sx.b);
  x = default_init;
  CHECK_ZERO_BYTES (&x);
  x = zero_int;
  CHECK_INT_BYTES (&x);
  x = s_default_init.a;
  CHECK_ZERO_BYTES (&x);
  x = s_default_init.b;
  CHECK_ZERO_BYTES (&x);
  exit (0);
}
