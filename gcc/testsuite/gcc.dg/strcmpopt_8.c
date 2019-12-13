/* PR tree-optimization/92683 - strncmp incorrect result with equal substrings
   and nonconst bound
   { dg-do compile }
   { dg-options "-O1 -Wall -fdump-tree-forwprop1" } */

#define SIZE_MAX  __SIZE_MAX__

#define S123  "123"
#define S1234 "1234"

typedef __SIZE_TYPE__ size_t;

#ifndef ident
#  define ident(n) n
#endif

extern void failure_on_line (int);

/* Verify that the test in 'if (EQL strncmp (S, T, N))' is folded.  */
#define T(eql, s, t, n) do {			\
    max = ident (n);				\
    if (!(eql __builtin_strncmp (s, t, max)))	\
      failure_on_line (__LINE__);		\
  } while (0)

void test_literal (void)
{
  size_t max;

  T (0 ==, S123, S1234, 0);
  T (0 ==, S123, S1234, 1);
  T (0 ==, S123, S1234, 2);
  T (0 ==, S123, S1234, 3);
  T (0 >,  S123, S1234, 4);
  T (0 >,  S123, S1234, 5);
  T (0 >,  S123, S1234, SIZE_MAX - 2);
  T (0 >,  S123, S1234, SIZE_MAX - 1);
  T (0 >,  S123, S1234, SIZE_MAX);

  T (0 ==, S123 + 1, S1234, 0);
  T (0 <,  S123 + 1, S1234, 1);
  T (0 <,  S123 + 1, S1234, 2);
  T (0 <,  S123 + 1, S1234, 3);
  T (0 <,  S123 + 1, S1234, 4);
  T (0 <,  S123 + 1, S1234, SIZE_MAX - 2);
  T (0 <,  S123 + 1, S1234, SIZE_MAX - 1);
  T (0 <,  S123 + 1, S1234, SIZE_MAX);

  T (0 ==, S123 + 1, S1234 + 1, 0);
  T (0 ==, S123 + 1, S1234 + 1, 1);
  T (0 ==, S123 + 1, S1234 + 1, 2);
  T (0 >,  S123 + 1, S1234 + 1, 3);
  T (0 >,  S123 + 1, S1234 + 1, SIZE_MAX - 1);
  T (0 >,  S123 + 1, S1234 + 1, SIZE_MAX);

  T (0 ==, S123 + 3, S1234 + 1, 0);
  T (0 >,  S123 + 3, S1234 + 1, 1);
  T (0 >,  S123 + 3, S1234 + 1, 2);
  T (0 >,  S123 + 3, S1234 + 1, 3);
  T (0 >,  S123 + 3, S1234 + 1, SIZE_MAX - 1);
  T (0 >,  S123 + 3, S1234 + 1, SIZE_MAX);

  int zero = 0;

  T (zero ==, S123, S1234, 0);
  T (zero ==, S123, S1234, 1);
  T (zero ==, S123, S1234, 2);
  T (zero ==, S123, S1234, 3);
  T (zero >,  S123, S1234, 4);
  T (zero >,  S123, S1234, 5);
  T (zero >,  S123, S1234, SIZE_MAX - 2);
  T (zero >,  S123, S1234, SIZE_MAX - 1);
  T (zero >,  S123, S1234, SIZE_MAX);

  T (zero ==, S123 + 1, S1234, 0);
  T (zero <,  S123 + 1, S1234, 1);
  T (zero <,  S123 + 1, S1234, 2);
  T (zero <,  S123 + 1, S1234, 3);
  T (zero <,  S123 + 1, S1234, 4);
  T (zero <,  S123 + 1, S1234, SIZE_MAX - 2);
  T (zero <,  S123 + 1, S1234, SIZE_MAX - 1);
  T (zero <,  S123 + 1, S1234, SIZE_MAX);

  T (zero ==, S123 + 1, S1234 + 1, 0);
  T (zero ==, S123 + 1, S1234 + 1, 1);
  T (zero ==, S123 + 1, S1234 + 1, 2);
  T (zero >,  S123 + 1, S1234 + 1, 3);
  T (zero >,  S123 + 1, S1234 + 1, SIZE_MAX - 1);
  T (zero >,  S123 + 1, S1234 + 1, SIZE_MAX);

  T (zero ==, S123 + 3, S1234 + 1, 0);
  T (zero >,  S123 + 3, S1234 + 1, 1);
  T (zero >,  S123 + 3, S1234 + 1, 2);
  T (zero >,  S123 + 3, S1234 + 1, 3);
  T (zero >,  S123 + 3, S1234 + 1, SIZE_MAX - 1);
  T (zero >,  S123 + 3, S1234 + 1, SIZE_MAX);
}

const char s123[] = S123;
const char s1234[] = S1234;

void test_cst_array (void)
{
  size_t max;

  T (0 ==, s123, s1234, 0);
  T (0 ==, s123, s1234, 1);
  T (0 ==, s123, s1234, 2);
  T (0 ==, s123, s1234, 3);
  T (0 >,  s123, s1234, 4);
  T (0 >,  s123, s1234, 5);
  T (0 >,  s123, s1234, SIZE_MAX - 2);
  T (0 >,  s123, s1234, SIZE_MAX - 1);
  T (0 >,  s123, s1234, SIZE_MAX);

  T (0 ==, s123 + 1, s1234, 0);
  T (0 <,  s123 + 1, s1234, 1);
  T (0 <,  s123 + 1, s1234, 2);
  T (0 <,  s123 + 1, s1234, 3);
  T (0 <,  s123 + 1, s1234, 4);
  T (0 <,  s123 + 1, s1234, SIZE_MAX - 2);
  T (0 <,  s123 + 1, s1234, SIZE_MAX - 1);
  T (0 <,  s123 + 1, s1234, SIZE_MAX);

  T (0 ==, s123 + 1, s1234 + 1, 0);
  T (0 ==, s123 + 1, s1234 + 1, 1);
  T (0 ==, s123 + 1, s1234 + 1, 2);
  T (0 >,  s123 + 1, s1234 + 1, 3);
  T (0 >,  s123 + 1, s1234 + 1, SIZE_MAX - 1);
  T (0 >,  s123 + 1, s1234 + 1, SIZE_MAX);

  T (0 ==, s123 + 3, s1234 + 1, 0);
  T (0 >,  s123 + 3, s1234 + 1, 1);
  T (0 >,  s123 + 3, s1234 + 1, 2);
  T (0 >,  s123 + 3, s1234 + 1, 3);
  T (0 >,  s123 + 3, s1234 + 1, SIZE_MAX - 1);
  T (0 >,  s123 + 3, s1234 + 1, SIZE_MAX);

  int zero = 0;

  T (zero ==, s123, s1234, 0);
  T (zero ==, s123, s1234, 1);
  T (zero ==, s123, s1234, 2);
  T (zero ==, s123, s1234, 3);
  T (zero >,  s123, s1234, 4);
  T (zero >,  s123, s1234, 5);
  T (zero >,  s123, s1234, SIZE_MAX - 2);
  T (zero >,  s123, s1234, SIZE_MAX - 1);
  T (zero >,  s123, s1234, SIZE_MAX);

  T (zero ==, s123 + 1, s1234, 0);
  T (zero <,  s123 + 1, s1234, 1);
  T (zero <,  s123 + 1, s1234, 2);
  T (zero <,  s123 + 1, s1234, 3);
  T (zero <,  s123 + 1, s1234, 4);
  T (zero <,  s123 + 1, s1234, SIZE_MAX - 2);
  T (zero <,  s123 + 1, s1234, SIZE_MAX - 1);
  T (zero <,  s123 + 1, s1234, SIZE_MAX);

  T (zero ==, s123 + 1, s1234 + 1, 0);
  T (zero ==, s123 + 1, s1234 + 1, 1);
  T (zero ==, s123 + 1, s1234 + 1, 2);
  T (zero >,  s123 + 1, s1234 + 1, 3);
  T (zero >,  s123 + 1, s1234 + 1, SIZE_MAX - 1);
  T (zero >,  s123 + 1, s1234 + 1, SIZE_MAX);

  T (zero ==, s123 + 3, s1234 + 1, 0);
  T (zero >,  s123 + 3, s1234 + 1, 1);
  T (zero >,  s123 + 3, s1234 + 1, 2);
  T (zero >,  s123 + 3, s1234 + 1, 3);
  T (zero >,  s123 + 3, s1234 + 1, SIZE_MAX - 1);
  T (zero >,  s123 + 3, s1234 + 1, SIZE_MAX);
}

/* { dg-final { scan-tree-dump-not "strcmp" "forwprop1" } }
   { dg-final { scan-tree-dump-not "strncmp" "forwprop1" } }
   { dg-final { scan-tree-dump-not "failure_on_line_" "forwprop1" } } */
