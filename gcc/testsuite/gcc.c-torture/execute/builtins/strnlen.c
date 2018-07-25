/* PR tree-optimization/81384 - built-in form of strnlen missing
   Test to verify that strnlen built-in expansion works correctly
   in the absence of tree strlen optimization.
   { dg-do run }
   { do-options "-O2 -fno-tree-strlen" }  */

#define PTRDIFF_MAX __PTRDIFF_MAX__
#define SIZE_MAX    __SIZE_MAX__
#define NOIPA __attribute__ ((noipa))

typedef __SIZE_TYPE__ size_t;

extern void abort (void);
extern size_t strnlen (const char *, size_t);

#define A(expr)							\
  ((expr) ? (void)0						\
   : (__builtin_printf ("assertion on line %i failed: %s\n",	\
			__LINE__, #expr),			\
      abort ()))

NOIPA void test_strnlen_str_cst (void)
{
  A (strnlen ("", 0) == 0);
  A (strnlen ("", 1) == 0);
  A (strnlen ("", 9) == 0);
  A (strnlen ("", PTRDIFF_MAX) == 0);
  A (strnlen ("", SIZE_MAX) == 0);
  A (strnlen ("", -1) == 0);

  A (strnlen ("1", 0) == 0);
  A (strnlen ("1", 1) == 1);
  A (strnlen ("1", 9) == 1);
  A (strnlen ("1", PTRDIFF_MAX) == 1);
  A (strnlen ("1", SIZE_MAX) == 1);
  A (strnlen ("1", -2) == 1);

  A (strnlen ("123", 0) == 0);
  A (strnlen ("123", 1) == 1);
  A (strnlen ("123", 2) == 2);
  A (strnlen ("123", 3) == 3);
  A (strnlen ("123", 9) == 3);
  A (strnlen ("123", PTRDIFF_MAX) == 3);
  A (strnlen ("123", SIZE_MAX) == 3);
  A (strnlen ("123", -2) == 3);
}

NOIPA void test_strnlen_str_range (size_t x)
{
  size_t r_0_3 = x & 3;
  size_t r_1_3 = r_0_3 | 1;
  size_t r_2_3 = r_0_3 | 2;

  A (strnlen ("",     r_0_3) == 0);
  A (strnlen ("1",    r_0_3) <= 1);
  A (strnlen ("12",   r_0_3) <= 2);
  A (strnlen ("123",  r_0_3) <= 3);
  A (strnlen ("1234", r_0_3) <= 3);

  A (strnlen ("",     r_1_3) == 0);
  A (strnlen ("1",    r_1_3) == 1);
  A (strnlen ("12",   r_1_3) <= 2);
  A (strnlen ("123",  r_1_3) <= 3);
  A (strnlen ("1234", r_1_3) <= 3);

  A (strnlen ("",     r_2_3) == 0);
  A (strnlen ("1",    r_2_3) == 1);
  A (strnlen ("12",   r_2_3) == 2);
  A (strnlen ("123",  r_2_3) <= 3);
  A (strnlen ("1234", r_2_3) <= 3);
}

NOIPA void test_strnlen_str_range_side_effect (size_t x)
{
  size_t r_0_3 = x & 3;
  size_t r_1_3 = r_0_3 | 1;
  size_t r_2_3 = r_0_3 | 2;
  size_t n = r_2_3;

  int i = 0;

  A (strnlen ("1234" + i++, n) <= 3);
  A (i == 1);

  A (strnlen ("1234", n++) <= 3);
  A (n == r_2_3 + 1);
}

void
main_test (void)
{
  test_strnlen_str_cst ();
  test_strnlen_str_range ((size_t)"");
  test_strnlen_str_range_side_effect ((size_t)"");
}
