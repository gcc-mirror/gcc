/* { dg-do compile } */
/* { dg-options "-std=gnu2y" } */

#define SCHAR_MAX  __SCHAR_MAX__
#define SCHAR_MIN  (-SCHAR_MAX - 1)
#define UCHAR_MAX  (SCHAR_MAX * 2 + 1)

#define SHRT_MAX   __SHRT_MAX__
#define SHRT_MIN   (-SHRT_MAX - 1)
#define USHRT_MAX  (SHRT_MAX * 2U + 1)

#define INT_MAX    __INT_MAX__
#define INT_MIN    (-INT_MAX - 1)
#define UINT_MAX   (INT_MAX * 2U + 1)

#define LONG_MAX   __LONG_MAX__
#define LONG_MIN   (-LONG_MAX - 1L)
#define ULONG_MAX  (LONG_MAX * 2LU + 1)

void
integer (void)
{
  _Static_assert (_Maxof (char) == SCHAR_MAX || _Maxof (char) == UCHAR_MAX);
  _Static_assert (_Minof (char) == SCHAR_MIN || _Minof (char) == 0);

  _Static_assert (_Maxof (signed char) == SCHAR_MAX);
  _Static_assert (_Maxof (short) == SHRT_MAX);
  _Static_assert (_Maxof (int) == INT_MAX);
  _Static_assert (_Maxof (long) == LONG_MAX);
  _Static_assert (_Maxof (long long) >= LONG_MAX);

  _Static_assert (_Minof (signed char) == SCHAR_MIN);
  _Static_assert (_Minof (short) == SHRT_MIN);
  _Static_assert (_Minof (int) == INT_MIN);
  _Static_assert (_Minof (long) == LONG_MIN);
  _Static_assert (_Minof (long long) <= LONG_MIN);

  _Static_assert (_Maxof (unsigned char) == UCHAR_MAX);
  _Static_assert (_Maxof (unsigned short) == USHRT_MAX);
  _Static_assert (_Maxof (unsigned int) == UINT_MAX);
  _Static_assert (_Maxof (unsigned long) == ULONG_MAX);
  _Static_assert (_Maxof (unsigned long long) >= ULONG_MAX);

  _Static_assert (_Minof (unsigned char) == 0);
  _Static_assert (_Minof (unsigned short) == 0);
  _Static_assert (_Minof (unsigned int) == 0);
  _Static_assert (_Minof (unsigned long) == 0);
  _Static_assert (_Minof (unsigned long long) == 0);

  _Static_assert (_Maxof (bool) == true);
  _Static_assert (_Minof (bool) == false);
}

void
enums (void)
{
  enum e1 { E1 };
  enum e2 : short { E2 };

  _Maxof (enum e1);
  _Minof (enum e1);
  _Static_assert (_Maxof (enum e2) == SHRT_MAX);
  _Static_assert (_Minof (enum e2) == SHRT_MIN);
}

void
expr (void)
{
  int x;

  _Maxof (x);  /* { dg-error "to something not a type" } */
  /* { dg-error "expected '\\)'" "syntax error" { target *-*-* } .-1 } */
  _Minof (x);  /* { dg-error "to something not a type" } */
  /* { dg-error "expected '\\)'" "syntax error" { target *-*-* } .-1 } */
  _Maxof (1);  /* { dg-error "to something not a type" } */
  /* { dg-error "expected '\\)'" "syntax error" { target *-*-* } .-1 } */
  _Minof (1);  /* { dg-error "to something not a type" } */
  /* { dg-error "expected '\\)'" "syntax error" { target *-*-* } .-1 } */
  _Maxof 1;  /* { dg-error "expected '\\('" } */
  _Minof 1;  /* { dg-error "expected '\\('" } */
  _Maxof (int) {1};  /* { dg-error "expected ';'" } */
  _Minof (int) {1};  /* { dg-error "expected ';'" } */
}

void
incomplete (void)
{
  _Maxof (enum e);  /* { dg-error "to incomplete type" } */
  _Minof (enum e);  /* { dg-error "to incomplete type" } */
}

void
non_int (void)
{
  struct s {int x;};
  union u {int x;};

  _Maxof (struct s);  /* { dg-error "to type" } */
  _Minof (struct s);  /* { dg-error "to type" } */
  _Maxof (union u);  /* { dg-error "to type" } */
  _Minof (union u);  /* { dg-error "to type" } */
  _Maxof (int [1]);  /* { dg-error "to type" } */
  _Minof (int [1]);  /* { dg-error "to type" } */
}

void
specs (void)
{
  _Maxof (static int);  /* { dg-error "to something not a type" } */
  /* { dg-error "expected '\\)'" "syntax error" { target *-*-* } .-1 } */
  _Minof (static int);  /* { dg-error "to something not a type" } */
  /* { dg-error "expected '\\)'" "syntax error" { target *-*-* } .-1 } */
  _Maxof (alignas(8) int);  /* { dg-error "alignment specified" } */
  _Minof (alignas(8) int);  /* { dg-error "alignment specified" } */
}

void
bogus (void)
{
  _Maxof (int x);  /* { dg-error "expected '\\)'" } */
  _Minof (int x);  /* { dg-error "expected '\\)'" } */
  _Maxof (int (!));  /* { dg-error "expected '\\)'" } */
  _Minof (int (!));  /* { dg-error "expected '\\)'" } */
}

void
type (void)
{
  _Generic (_Maxof (char), char: 0);
  _Generic (_Minof (char), char: 0);

  _Generic (_Maxof (signed char), signed char: 0);
  _Generic (_Maxof (short), short: 0);
  _Generic (_Maxof (int), int: 0);
  _Generic (_Maxof (long), long: 0);
  _Generic (_Maxof (long long), long long: 0);

  _Generic (_Minof (signed char), signed char: 0);
  _Generic (_Minof (short), short: 0);
  _Generic (_Minof (int), int: 0);
  _Generic (_Minof (long), long: 0);
  _Generic (_Minof (long long), long long: 0);

  _Generic (_Maxof (unsigned char), unsigned char: 0);
  _Generic (_Maxof (unsigned short), unsigned short: 0);
  _Generic (_Maxof (unsigned int), unsigned int: 0);
  _Generic (_Maxof (unsigned long), unsigned long: 0);
  _Generic (_Maxof (unsigned long long), unsigned long long: 0);

  _Generic (_Minof (unsigned char), unsigned char: 0);
  _Generic (_Minof (unsigned short), unsigned short: 0);
  _Generic (_Minof (unsigned int), unsigned int: 0);
  _Generic (_Minof (unsigned long), unsigned long: 0);
  _Generic (_Minof (unsigned long long), unsigned long long: 0);

  _Generic (_Maxof (bool), bool: 0);
  _Generic (_Minof (bool), bool: 0);
}
