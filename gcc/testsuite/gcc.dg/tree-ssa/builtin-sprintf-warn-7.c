/* PR middle-end/78786 - GCC hangs/out of memory calling sprintf with large
   precision
   { dg-do compile }
   { dg-require-effective-target int32plus }
   { dg-options "-Wformat-overflow -ftrack-macro-expansion=0" } */

#define INT_MAX __INT_MAX__
#define INT_MIN (-INT_MAX - 1)

typedef __SIZE_TYPE__ size_t;

void sink (int, void*);

char buf [1];

#define T(n, fmt, ...)					\
  sink (__builtin_sprintf (buf + sizeof buf - n, fmt, __VA_ARGS__), buf)

void test_integer_cst (void)
{
  T (0, "%*d",  INT_MIN, 0);     /* { dg-warning "writing 2147483648 bytes" } */
  T (0, "%*d",  INT_MAX, 0);     /* { dg-warning "writing 2147483647 bytes" } */

  T (0, "%.*d", INT_MIN, 0);     /* { dg-warning "writing 1 byte" } */
  T (0, "%.*d", INT_MAX, 0);     /* { dg-warning "writing 2147483647 bytes" } */

  T (0, "%*.*d", INT_MIN, INT_MIN, 0);   /* { dg-warning "writing 2147483648 bytes" } */

  T (0, "%*.*d", INT_MAX, INT_MAX, 0);   /* { dg-warning "writing 2147483647 bytes" } */
}

void test_integer_var (int i)
{
  T (0, "%*d",  INT_MIN, i);     /* { dg-warning "writing 2147483648 bytes" } */

  /* The following writes INT_MAX digits and, when i is negative, a minus
     sign.  */
  T (0, "%.*d", INT_MAX, i);     /* { dg-warning "writing between 2147483647 and 2147483648 bytes" } */

  T (0, "%.*d", INT_MIN, i);     /* { dg-warning "writing between 1 and 11 bytes" } */

  /* The following writes a range because of the possible minus sign.  */
  T (0, "%.*d", INT_MAX, i);     /* { dg-warning "writing between 2147483647 and 2147483648 bytes" } */

  T (0, "%*.*d", INT_MIN, INT_MIN, i);   /* { dg-warning "writing 2147483648 bytes" } */

  /* The following writes INT_MAX digits and, when i is negative, a minus
     sign.  */
  T (0, "%*.*d", INT_MAX, INT_MAX, i);   /* { dg-warning "writing between 2147483647 and 2147483648 bytes" } */
}

void test_floating_a_cst (void)
{
  T (0, "%*a",  INT_MIN, 0.);     /* { dg-warning "writing 2147483648 bytes" } */
  T (0, "%*a",  INT_MAX, 0.);     /* { dg-warning "writing 2147483647 bytes" } */

  /* %a is poorly specified and as a result some implementations trim
     redundant trailing zeros (e.g., Glibc) and others don't (e.g.,
     Solaris).  */
  T (0, "%.*a", INT_MIN, 0.);     /* { dg-warning "writing between 6 and 20 bytes" } */

  T (0, "%.*a", INT_MAX, 0.);     /* { dg-warning "writing 2147483654 bytes" } */

  T (0, "%*.*a", INT_MIN, INT_MIN, 0.);   /* { dg-warning "writing 2147483648 bytes" } */

  T (0, "%*.*a", INT_MAX, INT_MAX, 0.);   /* { dg-warning "writing 2147483654 bytes" } */
}

void test_floating_a_var (double x)
{
  T (0, "%*a",  INT_MIN, x);     /* { dg-warning "writing 2147483648 bytes" } */
  T (0, "%*a",  INT_MAX, x);     /* { dg-warning "writing 2147483647 bytes" } */

  T (0, "%.*a", INT_MIN, x);     /* { dg-warning "writing between 6 and 24 bytes" } */

  /* Expected output is "0x0." followed by INT_MAX digits followed by
     "p+" followed by 1 to four digits, with a byte count in the range
     [3 + INT_MAX + 2 + 1, 3 + INT_MAX + 2 + 4].  */
  T (0, "%.*a", INT_MAX, x);     /* { dg-warning "writing between 2147483654 and 2147483658 bytes" } */

  T (0, "%*.*a", INT_MIN, INT_MIN, x);   /* { dg-warning "writing 2147483648 bytes" } */

  T (0, "%*.*a", INT_MAX, INT_MAX, x);   /* { dg-warning "writing between 2147483654 and 2147483658 bytes" } */
}

void test_floating_e_cst (void)
{
  T (0, "%*e",  INT_MIN, 0.);     /* { dg-warning "writing 2147483648 bytes" } */
  T (0, "%*e",  INT_MAX, 0.);     /* { dg-warning "writing 2147483647 bytes" } */

  T (0, "%.*e", INT_MIN, 0.);     /* { dg-warning "writing 12 bytes" } */

  T (0, "%.*e", INT_MAX, 0.);     /* { dg-warning "writing 2147483653 bytes" } */

  T (0, "%*.*e", INT_MIN, INT_MIN, 0.);   /* { dg-warning "writing 2147483648 bytes" } */

  T (0, "%*.*e", INT_MAX, INT_MAX, 0.);   /* { dg-warning "writing 2147483653 bytes" } */
}

void test_floating_e_var (double x)
{
  T (0, "%*e",  INT_MIN, x);     /* { dg-warning "writing 2147483648 bytes" } */
  T (0, "%*e",  INT_MAX, x);     /* { dg-warning "writing 2147483647 bytes" } */

  T (0, "%.*e", INT_MIN, x);     /* { dg-warning "writing between 12 and 14 bytes" } */

  T (0, "%.*e", INT_MAX, x);     /* { dg-warning "writing between 2147483653 and 2147483655 bytes" } */

  T (0, "%*.*e", INT_MIN, INT_MIN, x);   /* { dg-warning "writing 2147483648 bytes" } */

  T (0, "%*.*e", INT_MAX, INT_MAX, x);   /* { dg-warning "writing between 2147483653 and 2147483655 bytes" } */
}

void test_floating_f_cst (void)
{
  T (0, "%*f",  INT_MIN, 0.);     /* { dg-warning "writing 2147483648 bytes" } */
  T (0, "%*f",  INT_MAX, 0.);     /* { dg-warning "writing 2147483647 bytes" } */

  T (0, "%.*f", INT_MIN, 0.);     /* { dg-warning "writing 8 bytes" } */

  T (0, "%.*f", INT_MAX, 0.);     /* { dg-warning "writing 2147483649 bytes" } */

  T (0, "%*.*f", INT_MIN, INT_MIN, 0.);   /* { dg-warning "writing 2147483648 bytes" } */

  T (0, "%*.*f", INT_MAX, INT_MAX, 0.);   /* { dg-warning "writing 2147483649 bytes" } */
}

void test_floating_f_var (double x)
{
  T (0, "%*f",  INT_MIN, x);     /* { dg-warning "writing 2147483648 bytes" } */
  T (0, "%*f",  INT_MAX, x);     /* { dg-warning "writing 2147483647 bytes" } */

  T (0, "%.*f", INT_MIN, x);     /* { dg-warning "writing between 8 and 317 bytes" } */

  T (0, "%.*f", INT_MAX, x);     /* { dg-warning "writing between 2147483649 and 2147483958 bytes" } */

  T (0, "%*.*f", INT_MIN, INT_MIN, x);   /* { dg-warning "writing 2147483648 bytes" } */

  T (0, "%*.*f", INT_MAX, INT_MAX, x);   /* { dg-warning "writing between 2147483649 and 2147483958 bytes" } */
}

void test_floating_g_cst (void)
{
  T (0, "%*g",  INT_MIN, 0.);     /* { dg-warning "writing 2147483648 bytes" } */
  T (0, "%*g",  INT_MAX, 0.);     /* { dg-warning "writing 2147483647 bytes" } */

  T (0, "%.*g", INT_MIN, 0.);     /* { dg-warning "writing 1 byte" } */

  T (0, "%.*g", INT_MAX, 0.);     /* { dg-warning "writing 1 byte" } */

  T (0, "%*.*g", INT_MIN, INT_MIN, 0.);   /* { dg-warning "writing 2147483648 bytes" } */

  T (0, "%*.*g", INT_MAX, INT_MAX, 0.);   /* { dg-warning "writing 2147483647 bytes" } */
}

void test_floating_g (double x)
{
  T (0, "%*g",  INT_MIN, x);     /* { dg-warning "writing 2147483648 bytes" } */
  T (0, "%*g",  INT_MAX, x);     /* { dg-warning "writing 2147483647 bytes" } */

  T (0, "%.*g", INT_MIN, x);     /* { dg-warning "writing between 1 and 13 bytes" } */

  T (0, "%.*g", INT_MAX, x);     /* { dg-warning "writing between 1 and 310 bytes" } */

  T (0, "%*.*g", INT_MIN, INT_MIN, x);   /* { dg-warning "writing 2147483648 bytes" } */

  T (0, "%*.*g", INT_MAX, INT_MAX, x);   /* { dg-warning "writing 2147483647 bytes" } */
}

void test_string_cst (void)
{
  T (0, "%*s",  INT_MIN, "");     /* { dg-warning "writing 2147483648 bytes" } */
  T (0, "%*s",  INT_MAX, "");     /* { dg-warning "writing 2147483647 bytes" } */

  T (0, "%.*s", INT_MIN, "");     /* { dg-warning "writing a terminating nul" } */

  T (0, "%.*s", INT_MAX, "");     /* { dg-warning "writing a terminating nul" } */

  T (0, "%*.*s", INT_MIN, INT_MIN, "");   /* { dg-warning "writing 2147483648 bytes" } */

  T (0, "%*.*s", INT_MAX, INT_MAX, "");   /* { dg-warning "writing 2147483647 bytes" } */
}

void test_string_var (const char *s)
{
  T (0, "%*s",  INT_MIN, s);     /* { dg-warning "writing 2147483648 or more bytes" } */
  T (0, "%*s",  INT_MAX, s);     /* { dg-warning "writing 2147483647 or more bytes" } */

  T (0, "%.*s", INT_MIN, s);     /* { dg-warning "writing a terminating nul" } */

  T (0, "%.*s", INT_MAX, s);     /* { dg-warning "writing up to 2147483647 bytes" } */

  T (0, "%*.*s", INT_MIN, INT_MIN, s);   /* { dg-warning "writing 2147483648 or more bytes" } */

  T (0, "%*.*s", INT_MAX, INT_MAX, s);   /* { dg-warning "writing 2147483647 bytes" } */
}
