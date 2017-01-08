/* { dg-do compile } */
/* { dg-options "-O2 -Wformat -Wformat-truncation=1 -ftrack-macro-expansion=0" } */

typedef struct
{
  char a0[0];
  char a1[1];
  char a2[2];
  char a3[3];
  char a4[4];
  char ax[];
} Arrays;

char buffer[1024];
#define buffer(size) (buffer + sizeof buffer - size)

int value_range (int min, int max)
{
  extern int value (void);
  int val = value ();
  return val < min || max < val ? min : val;
}

#define R(min, max)  value_range (min, max)

/* Verify that calls to snprintf whose return value is unused are
   diagnosed if certain or possible truncation is detected.  */

#define T(size, ...) \
  __builtin_snprintf (buffer (size), size, __VA_ARGS__)

void test_int_retval_unused (void)
{
  T (2, "%i", 123);          /* { dg-warning "output truncated" } */
  T (2, "%i", R (1, 99));    /* { dg-warning "output may be truncated" } */
  T (2, "%i", R (10, 99));   /* { dg-warning "output truncated" } */
  T (3, "%i%i", R (1, 99), R (1, 99));   /* { dg-warning "output may be truncated" } */
}

void test_string_retval_unused (const Arrays *ar)
{
  T (1, "%-s", ar->a0);
  T (1, "%-s", ar->a1);
  T (1, "%-s", ar->a2);   /* { dg-warning "output may be truncated" } */
}


/* Verify that calls to snprintf whose return value is used are
   diagnosed only if certain truncation is detected but not when
   truncation is only possible but not certain.  */

volatile int retval;

#undef T
#define T(size, ...) \
  retval = __builtin_snprintf (buffer (size), size, __VA_ARGS__)

void test_int_retval_used (void)
{
  T (2, "%i", 123);          /* { dg-warning "output truncated" } */
  T (2, "%i", R (1, 99));
  T (2, "%i", R (10, 99));   /* { dg-warning "output truncated" } */
  T (3, "%i%i", R (1, 99), R (1, 99));
}

void test_string_retval_used (const Arrays *ar)
{
  T (1, "%-s", ar->a0);
  T (1, "%-s", ar->a1);
  T (1, "%-s", ar->a2);
  T (1, "%-s", ar->a4);
  T (1, "%-s", "123");   /* { dg-warning "output truncated" } */
}
