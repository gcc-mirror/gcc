/* Test precedence and associativity in expressions.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "" } */

struct s { int *a; } *p, q, *r[2], *g();
int *i[2];
int j[2];
_Complex double c[2];

void
f (void)
{
#define ASSERT(expr) do { char x[(expr) ? 1 : -1]; } while (0)
  /* Postfix and unary operators and casts.  */
  *p++;
  *p--;
  *p->a;
  *q.a;
  *r[1];
  (*g()).a;
  ++i[0];
  --i[0];
  +j[0];
  -j[0];
  ~j[0];
  !j[0];
  __real__ c[1];
  __imag__ c[1];
  ASSERT ((sizeof p++) == (sizeof p));
  ASSERT ((sizeof (int) + 1) == ((sizeof (int)) + 1));
  ASSERT ((__alignof p++) == (__alignof p));
  ASSERT ((__alignof (int) + 1) == ((__alignof (int)) + 1));
  ASSERT ((sizeof __extension__ 1 + 1) == ((sizeof 1) + 1));
  /* Binary operators.  */
  ASSERT (((_Bool) 1 * 2) == 2);
  ASSERT (((_Bool) 8 / 4) == 0);
  ASSERT (((_Bool) 8 % 4) == 1);
#define ASSERT_BIN(C1, O1, C2, O2, C3, V1, V2, V3)	\
  do {							\
    ASSERT ((C1 O1 C2 O2 C3) == V1);			\
    ASSERT (((C1 O1 C2) O2 C3) == V2);			\
    ASSERT ((C1 O1 (C2 O2 C3)) == V3);			\
  } while (0);
  ASSERT_BIN (1, *, 2, *, 3, 6, 6, 6);
  ASSERT_BIN (2, *, 2, /, 3, 1, 1, 0);
  ASSERT_BIN (2, *, 2, %, 3, 1, 1, 4);
  ASSERT_BIN (2, /, 2, *, 3, 3, 3, 0);
  ASSERT_BIN (2, /, 2, /, 2, 0, 0, 2);
  ASSERT_BIN (2, /, 4, %, 3, 0, 0, 2);
  ASSERT_BIN (2, %, 2, *, 3, 0, 0, 2);
  ASSERT_BIN (2, %, 9, /, 3, 0, 0, 2);
  ASSERT_BIN (2, %, 4, %, 3, 2, 2, 0);
  ASSERT_BIN (2, *, 3, +, 4, 10, 10, 14);
  ASSERT_BIN (2, *, 3, -, 4, 2, 2, -2);
  ASSERT_BIN (2, /, 3, +, 4, 4, 4, 0);
  ASSERT_BIN (2, /, 3, -, 4, -4, -4, -2);
  ASSERT_BIN (2, %, 3, +, 4, 6, 6, 2);
  ASSERT_BIN (2, %, 3, -, 4, -2, -2, 0);
  ASSERT_BIN (2, +, 3, *, 4, 14, 20, 14);
  ASSERT_BIN (2, +, 3, /, 4, 2, 1, 2);
  ASSERT_BIN (2, +, 3, %, 4, 5, 1, 5);
  ASSERT_BIN (2, -, 3, *, 4, -10, -4, -10);
  ASSERT_BIN (2, -, 3, /, 4, 2, 0, 2);
  ASSERT_BIN (2, -, 4, %, 4, 2, -2, 2);
  ASSERT_BIN (2, +, 3, +, 4, 9, 9, 9);
  ASSERT_BIN (2, +, 3, -, 4, 1, 1, 1);
  ASSERT_BIN (2, -, 3, +, 4, 3, 3, -5);
  ASSERT_BIN (2, -, 3, -, 4, -5, -5, 3);
  ASSERT_BIN (3, +, 2, <<, 4, 80, 80, 35);
  ASSERT_BIN (3, +, 2, >>, 4, 0, 0, 3);
  ASSERT_BIN (3, -, 2, <<, 4, 16, 16, -29);
  ASSERT_BIN (3, -, 2, >>, 4, 0, 0, 3);
  ASSERT_BIN (2, <<, 4, +, 3, 256, 35, 256);
  ASSERT_BIN (2, <<, 4, -, 3, 4, 29, 4);
  ASSERT_BIN (2, >>, 4, +, 3, 0, 3, 0);
  ASSERT_BIN (2, >>, 4, -, 3, 1, -3, 1);
  ASSERT_BIN (4L, <<, 2L, <<, 3L, 128L, 128L, 262144L);
  ASSERT_BIN (4L, <<, 2L, >>, 3L, 2L, 2L, 4L);
  ASSERT_BIN (4L, >>, 2L, <<, 3L, 8L, 8L, 0L);
  ASSERT_BIN (4L, >>, 2L, >>, 3L, 0L, 0L, 4L);
  ASSERT_BIN (2, <<, 5, <, 4, 0, 0, 2);
  ASSERT_BIN (2, <<, 5, >, 4, 1, 1, 4);
  ASSERT_BIN (2, <<, 5, <=, 4, 0, 0, 2);
  ASSERT_BIN (2, <<, 5, >=, 4, 1, 1, 4);
  ASSERT_BIN (2, >>, 5, <, 4, 1, 1, 2);
  ASSERT_BIN (2, >>, 5, >, 4, 0, 0, 1);
  ASSERT_BIN (2, >>, 5, <=, 4, 1, 1, 2);
  ASSERT_BIN (2, >>, 5, >=, 4, 0, 0, 1);
  ASSERT_BIN (4, <, 3, <<, 2, 1, 0, 1);
  ASSERT_BIN (4, <, 20, >>, 2, 1, 0, 1);
  ASSERT_BIN (4, >, 3, <<, 2, 0, 4, 0);
  ASSERT_BIN (4, >, 3, >>, 2, 1, 0, 1);
  ASSERT_BIN (4, <=, 3, <<, 2, 1, 0, 1);
  ASSERT_BIN (4, <=, 20, >>, 2, 1, 0, 1);
  ASSERT_BIN (4, >=, 3, <<, 2, 0, 4, 0);
  ASSERT_BIN (4, >=, 3, >>, 2, 1, 0, 1);
  ASSERT_BIN (1, <, 2, <, 3, 1, 1, 0);
  ASSERT_BIN (1, <, 2, >, 0, 1, 1, 0);
  ASSERT_BIN (1, <, 2, <=, 3, 1, 1, 0);
  ASSERT_BIN (0, <, 4, >=, 3, 0, 0, 1);
  ASSERT_BIN (1, >, 2, <, 3, 1, 1, 0);
  ASSERT_BIN (1, >, 2, >, 3, 0, 0, 1);
  ASSERT_BIN (1, >, 2, <=, 3, 1, 1, 0);
  ASSERT_BIN (1, >, 2, >=, 3, 0, 0, 1);
  ASSERT_BIN (3, <=, 2, <, 3, 1, 1, 0);
  ASSERT_BIN (2, <=, 3, >, 0, 1, 1, 0);
  ASSERT_BIN (2, <=, 3, <=, 4, 1, 1, 0);
  ASSERT_BIN (2, <=, 3, >=, 1, 1, 1, 0);
  ASSERT_BIN (0, >=, 2, <, 3, 1, 1, 0);
  ASSERT_BIN (1, >=, 2, >, 3, 0, 0, 1);
  ASSERT_BIN (0, >=, 2, <=, 3, 1, 1, 0);
  ASSERT_BIN (1, >=, 2, >=, 3, 0, 0, 1);
  ASSERT_BIN (-1, <, 2, ==, 3, 0, 0, 1);
  ASSERT_BIN (1, <, 2, !=, 3, 1, 1, 0);
  ASSERT_BIN (1, >, 2, ==, 3, 0, 0, 1);
  ASSERT_BIN (1, >, 2, !=, 3, 1, 1, 0);
  ASSERT_BIN (0, <=, 2, ==, 3, 0, 0, 1);
  ASSERT_BIN (2, <=, 2, !=, 3, 1, 1, 0);
  ASSERT_BIN (1, >=, 2, ==, 3, 0, 0, 1);
  ASSERT_BIN (0, >=, 2, !=, 3, 1, 1, 0);
  ASSERT_BIN (1, ==, 3, <, 2, 0, 1, 0);
  ASSERT_BIN (1, ==, 3, >, 2, 1, 0, 1);
  ASSERT_BIN (1, ==, 3, <=, 2, 0, 1, 0);
  ASSERT_BIN (1, ==, 3, >=, 2, 1, 0, 1);
  ASSERT_BIN (1, !=, 2, <, 3, 0, 1, 0);
  ASSERT_BIN (1, !=, 2, >, 3, 1, 0, 1);
  ASSERT_BIN (1, !=, 2, <=, 3, 0, 1, 0);
  ASSERT_BIN (1, !=, 2, >=, 3, 1, 0, 1);
  ASSERT_BIN (1, ==, 2, ==, 0, 1, 1, 0);
  ASSERT_BIN (1, ==, 2, !=, 0, 0, 0, 1);
  ASSERT_BIN (1, !=, 2, ==, 3, 0, 0, 1);
  ASSERT_BIN (1, !=, 2, !=, 3, 1, 1, 0);
  ASSERT_BIN (0, ==, 2, &, 1, 0, 0, 1);
  ASSERT_BIN (0, !=, 2, &, 1, 1, 1, 0);
  ASSERT_BIN (1, &, 2, ==, 0, 0, 1, 0);
  ASSERT_BIN (1, &, 2, !=, 0, 1, 0, 1);
  ASSERT_BIN (1, &, 0x2, ^, 3, 3, 3, 1);
  ASSERT_BIN (3, ^, 2, &, 1, 3, 1, 3);
  ASSERT_BIN (3, ^, 2, |, 1, 1, 1, 0);
  ASSERT_BIN (3, |, 0x2, ^, 1, 3, 2, 3);
  ASSERT_BIN (2, |, 0, &&, 2, 1, 1, 2);
  ASSERT_BIN (2, &&, 0, |, 2, 1, 2, 1);
  ASSERT_BIN (0, &&, 0, ||, 1, 1, 1, 0);
  ASSERT_BIN (1, ||, 0, &&, 0, 1, 0, 1);
  /* Conditional expressions.  */
  ASSERT ((1 || 2 ? 3 : 4) == 3);
  ASSERT ((1 || (2 ? 3 : 4)) == 1);
  /* Assignment expressions.  */
  p = p = p;
  /* Expressions.  */
  p, p = p;
}
