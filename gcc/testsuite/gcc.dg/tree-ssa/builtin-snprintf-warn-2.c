/* { dg-do compile } */
/* { dg-options "-O2 -Wformat -Wformat-truncation=2 -ftrack-macro-expansion=0" } */

typedef struct
{
  char a0[0];
  /* Separate a0 from a1 to prevent the former from being substituted
     for the latter and causing false positives.  */
  int: 8;
  char a1[1];
  char a2[2];
  char a3[3];
  char a4[4];
  char ax[];
} Arrays;

char buffer[1024];
#define buffer(size) (buffer + sizeof buffer - size)

static int value_range (int min, int max)
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
  /* At level 2 strings of unknown length are assumed to be 1 character
     long, so the following is diagnosed.  */
  T (1, "%-s", ar->a0);   /* { dg-warning "output may be truncated" } */
  T (1, "%-s", ar->a1);
  T (1, "%-s", ar->a2);   /* { dg-warning "output may be truncated" } */
}


/* Verify that (at -Wformat-trunc=2) calls to snprintf whose return value
   is used are diagnosed the same way as those whose value is unused.  */

volatile int retval;

#undef T
#define T(size, ...) \
  retval = __builtin_snprintf (buffer (size), size, __VA_ARGS__)

void test_int_retval_used (void)
{
  T (2, "%i", 123);          /* { dg-warning "output truncated" } */
  T (2, "%i", R (1, 99));    /* { dg-warning "output may be truncated" } */
  T (2, "%i", R (10, 99));   /* { dg-warning "output truncated" } */
  T (3, "%i%i", R (1, 99), R (1, 99));   /* { dg-warning "output may be truncated" } */
}

void test_string_retval_used (const Arrays *ar)
{
  T (1, "%-s", ar->a0);   /* { dg-warning "output may be truncated" } */
  T (1, "%-s", ar->a1);
  T (1, "%-s", ar->a2);   /* { dg-warning "output may be truncated" } */
}
