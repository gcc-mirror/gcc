/* PR tree-optimization/ - fold strlen relational expressions
   { dg-do run }
   { dg-options "-O2 -Wall -Wno-unused-local-typedefs -fdump-tree-optimized" } */

typedef __SIZE_TYPE__ size_t;

#define NOIPA   __attribute__ ((noipa))

#define CONCAT(a, b) a ## b
#define CAT(a, b)    CONCAT (a, b)

/* Used in tests where EXPR is expected to be folded to false.  */
#define ELIM(expr)							\
  if (expr) {								\
    extern void								\
      CAT (CAT (test_on_line_, __LINE__), _not_eliminated)(void);	\
    CAT (CAT (test_on_line_, __LINE__), _not_eliminated)();		\
  } typedef void dummy_type

char a[32], b[32];

void init (void)
{
  __builtin_strncpy (a, "abcdefgh", sizeof a);
  __builtin_strncpy (b, "0123456789", sizeof b);
}

NOIPA void fail (const char *func)
{
  __builtin_printf ("failure in %s\n", func);
  __builtin_abort ();
}

NOIPA void test_global_cpy_4 (void)
{
  size_t blen = __builtin_strlen (b);
  if (blen < 9) return;

  char *d = a;
  __builtin_memcpy (d, b, 4);

  size_t dlen = __builtin_strlen (d);
  if (dlen != 8)   // cannot be eliminated
    fail ("test_global");
}


NOIPA void test_global_cpy_10 (void)
{
  size_t blen = __builtin_strlen (b);
  if (blen < 9) return;

  char *d = a;
  __builtin_memcpy (d, b, 10);

  size_t dlen = __builtin_strlen (d);
  if (dlen != 10)   // cannot be eliminated
    fail ("test_global_cpy_10");
}

NOIPA void test_global_cpy_11 (void)
{
  size_t blen = __builtin_strlen (b);
  if (blen < 9) return;

  char *d = a;
  __builtin_memcpy (d, b, 11);

  size_t dlen = __builtin_strlen (d);
  if (dlen != 10)   // cannot be eliminated
    fail ("test_global_cpy_11");
}

NOIPA void test_global_cpy_20 (void)
{
  size_t blen = __builtin_strlen (b);
  if (blen < 9) return;

  char *d = a;
  __builtin_memcpy (d, b, 20);

  size_t dlen = __builtin_strlen (d);
  if (dlen != 10)   // cannot be eliminated
    fail ("test_global_cpy_20");
}

NOIPA void test_local_cpy_4 (void)
{
  size_t blen = __builtin_strlen (b);
  if (blen < 9) return;

  char a[10] = "abcdefgh";
  char *d = a;
  __builtin_memcpy (d, b, 4);

  size_t dlen = __builtin_strlen (d);
  ELIM (dlen != 8);
}

NOIPA void test_local_cpy_10 (void)
{
  size_t blen = __builtin_strlen (b);
  if (blen < 9) return;

  char a[32] = "abcdefgh";
  char *d = a;
  __builtin_memcpy (d, b, 10);

  /* B can be longer than 9 and A can initially be longer than 10
     so the test below cannot be eliminated.  */
  size_t dlen = __builtin_strlen (d);
  if (dlen != 10)
    fail ("test_local_cpy_10");
}

NOIPA void test_local_cpy_11 (void)
{
  size_t blen = __builtin_strlen (b);
  if (blen < 9) return;

  char a[32] = "abcdefgh";
  char *d = a;
  __builtin_memcpy (d, b, 11);

  size_t dlen = __builtin_strlen (d);
  if (dlen != 10)
    fail ("test_global_cpy_20");
}

NOIPA void test_local_cpy_20 (void)
{
  size_t blen = __builtin_strlen (b);
  if (blen < 9) return;

  char a[32] = "abcdefgh";
  char *d = a;
  __builtin_memcpy (d, b, 20);

  size_t dlen = __builtin_strlen (d);
  if (dlen != 10)
    fail ("test_global_cpy_20");
}

NOIPA void test_global_length_eq (void)
{
  size_t blen = __builtin_strlen (b);
  if (blen != 10) return;

  size_t alen = __builtin_strlen (a);
  if (alen != 8) return;

  char *d = a;
  __builtin_memcpy (d, b, 4);

  size_t dlen = __builtin_strlen (d);
  ELIM (dlen != 8);
}


NOIPA void test_global_length_gt (void)
{
  size_t blen = __builtin_strlen (b);
  if (blen < 9) return;

  size_t alen = __builtin_strlen (a);
  if (alen < 8) return;

  char *d = a;
  __builtin_memcpy (d, b, 4);

  size_t dlen = __builtin_strlen (d);
  ELIM (dlen < 8);
}

#define TEST(name) do { init (); test_ ## name (); } while (0)

int main (void)
{
  TEST (local_cpy_4);
  TEST (local_cpy_10);
  TEST (local_cpy_11);
  TEST (local_cpy_20);

  TEST (global_cpy_4);
  TEST (global_cpy_10);
  TEST (global_cpy_11);
  TEST (global_cpy_20);
  TEST (global_length_eq);
  TEST (global_length_gt);
}
