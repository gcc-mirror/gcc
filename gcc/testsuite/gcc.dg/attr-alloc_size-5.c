/* PR c/78284 - warn on malloc with very large arguments
   Test exercising the ability to detect and diagnose calls to allocation
   functions decorated with attribute alloc_size that attempt to allocate
   zero bytes.  For standard allocation functions the return value is
   implementation-defined and so relying on it may be a source of bugs.  */
/* { dg-do compile } */
/* { dg-options "-O2 -Wall -Walloc-zero" } */

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

#define LLONG_MAX  __LLONG_MAX__
#define LLONG_MIN  (-LLONG_MAX - 1LL)
#define ULLONG_MAX (ULLONG_MAX * 2LLU + 1)

#define SIZE_MAX   __SIZE_MAX__

typedef __SIZE_TYPE__ size_t;


#define ALLOC_SIZE(...) __attribute__ ((alloc_size (__VA_ARGS__)))

void* f_uchar_1 (unsigned char) ALLOC_SIZE (1);
void* f_uchar_2 (unsigned char, unsigned char) ALLOC_SIZE (1, 2);
void* f_schar_1 (signed char) ALLOC_SIZE (1);
void* f_schar_2 (signed char, signed char) ALLOC_SIZE (1, 2);

void* f_ushrt_1 (unsigned short) ALLOC_SIZE (1);
void* f_ushrt_2 (unsigned short, unsigned short) ALLOC_SIZE (1, 2);
void* f_shrt_1 (signed short) ALLOC_SIZE (1);
void* f_shrt_2 (signed short, signed short) ALLOC_SIZE (1, 2);

void* f_uint_1 (unsigned) ALLOC_SIZE (1);
void* f_uint_2 (unsigned, unsigned) ALLOC_SIZE (1, 2);
void* f_int_1 (int) ALLOC_SIZE (1);
void* f_int_2 (int, int) ALLOC_SIZE (1, 2);

void* f_ulong_1 (unsigned long) ALLOC_SIZE (1);
void* f_ulong_2 (unsigned long, unsigned long) ALLOC_SIZE (1, 2);
void* f_long_1 (long) ALLOC_SIZE (1);
void* f_long_2 (long, long) ALLOC_SIZE (1, 2);

void* f_ullong_1 (unsigned long long) ALLOC_SIZE (1);
void* f_ullong_2 (unsigned long long, unsigned long long) ALLOC_SIZE (1, 2);
void* f_llong_1 (long long) ALLOC_SIZE (1);
void* f_llong_2 (long long, long long) ALLOC_SIZE (1, 2);

void* f_size_1 (size_t) ALLOC_SIZE (1);
void* f_size_2 (size_t, size_t) ALLOC_SIZE (1, 2);

void* f_size_1_nonnull (size_t)
     ALLOC_SIZE (1)  __attribute__ ((returns_nonnull));
void* f_size_2_nonnull (size_t, size_t)
     ALLOC_SIZE (1, 2) __attribute__ ((returns_nonnull));

void sink (void*);

void
test_uchar (unsigned char n)
{
  sink (f_uchar_1 (0));     /* { dg-warning "argument 1 value is zero" } */
  sink (f_uchar_2 (0, 1));  /* { dg-warning "argument 1 value is zero" } */
  sink (f_uchar_2 (1, 0));  /* { dg-warning "argument 2 value is zero" } */
  sink (f_uchar_2 (n, 0));  /* { dg-warning "argument 2 value is zero" } */
  sink (f_uchar_2 (0, n));  /* { dg-warning "argument 1 value is zero" } */

  sink (f_uchar_1 (n));
  n = 0;
  sink (f_uchar_1 (n));     /* { dg-warning "argument 1 value is zero" } */
  sink (f_uchar_2 (1, n));  /* { dg-warning "argument 2 value is zero" } */
}

void
test_schar (signed char n)
{
  sink (f_schar_1 (0));     /* { dg-warning "argument 1 value is zero" } */
  sink (f_schar_2 (0, 1));  /* { dg-warning "argument 1 value is zero" } */
  sink (f_schar_2 (1, 0));  /* { dg-warning "argument 2 value is zero" } */
  sink (f_schar_2 (n, 0));  /* { dg-warning "argument 2 value is zero" } */
  sink (f_schar_2 (0, n));  /* { dg-warning "argument 1 value is zero" } */

  sink (f_schar_1 (n));
  n = 0;
  sink (f_schar_1 (n));     /* { dg-warning "argument 1 value is zero" } */
  sink (f_schar_2 (1, n));  /* { dg-warning "argument 2 value is zero" } */
}

void
test_ushrt (unsigned short n)
{
  sink (f_ushrt_1 (0));     /* { dg-warning "argument 1 value is zero" } */
  sink (f_ushrt_2 (0, 1));  /* { dg-warning "argument 1 value is zero" } */
  sink (f_ushrt_2 (1, 0));  /* { dg-warning "argument 2 value is zero" } */
  sink (f_ushrt_2 (n, 0));  /* { dg-warning "argument 2 value is zero" } */
  sink (f_ushrt_2 (0, n));  /* { dg-warning "argument 1 value is zero" } */

  sink (f_ushrt_1 (n));
  n = 0;
  sink (f_ushrt_1 (n));     /* { dg-warning "argument 1 value is zero" } */
  sink (f_ushrt_2 (1, n));  /* { dg-warning "argument 2 value is zero" } */
}

void
test_shrt (short n)
{
  sink (f_shrt_1 (0));      /* { dg-warning "argument 1 value is zero" } */
  sink (f_shrt_2 (0, 1));   /* { dg-warning "argument 1 value is zero" } */
  sink (f_shrt_2 (1, 0));   /* { dg-warning "argument 2 value is zero" } */
  sink (f_shrt_2 (n, 0));   /* { dg-warning "argument 2 value is zero" } */
  sink (f_shrt_2 (0, n));   /* { dg-warning "argument 1 value is zero" } */

  sink (f_shrt_1 (n));
  n = 0;
  sink (f_shrt_1 (n));      /* { dg-warning "argument 1 value is zero" } */
  sink (f_shrt_2 (1, n));   /* { dg-warning "argument 2 value is zero" } */
}

void
test_uint (unsigned n)
{
  sink (f_uint_1 (0));      /* { dg-warning "argument 1 value is zero" } */
  sink (f_uint_2 (0, 1));   /* { dg-warning "argument 1 value is zero" } */
  sink (f_uint_2 (1, 0));   /* { dg-warning "argument 2 value is zero" } */
  sink (f_uint_2 (n, 0));   /* { dg-warning "argument 2 value is zero" } */
  sink (f_uint_2 (0, n));   /* { dg-warning "argument 1 value is zero" } */

  sink (f_uint_1 (n));
  n = 0;
  sink (f_uint_1 (n));      /* { dg-warning "argument 1 value is zero" } */
  sink (f_uint_2 (1, n));   /* { dg-warning "argument 2 value is zero" } */
}

void
test_int (int n)
{
  sink (f_int_1 (0));       /* { dg-warning "argument 1 value is zero" } */
  sink (f_int_2 (0, 1));    /* { dg-warning "argument 1 value is zero" } */
  sink (f_int_2 (1, 0));    /* { dg-warning "argument 2 value is zero" } */
  sink (f_int_2 (n, 0));    /* { dg-warning "argument 2 value is zero" } */
  sink (f_int_2 (0, n));    /* { dg-warning "argument 1 value is zero" } */

  sink (f_int_1 (n));
  n = 0;
  sink (f_int_1 (n));       /* { dg-warning "argument 1 value is zero" } */
  sink (f_int_2 (1, n));    /* { dg-warning "argument 2 value is zero" } */
}

void
test_ulong (unsigned long n)
{
  sink (f_ulong_1 (0));     /* { dg-warning "argument 1 value is zero" } */
  sink (f_ulong_2 (0, 1));  /* { dg-warning "argument 1 value is zero" } */
  sink (f_ulong_2 (1, 0));  /* { dg-warning "argument 2 value is zero" } */
  sink (f_ulong_2 (n, 0));  /* { dg-warning "argument 2 value is zero" } */
  sink (f_ulong_2 (0, n));  /* { dg-warning "argument 1 value is zero" } */

  sink (f_ulong_1 (n));
  n = 0;
  sink (f_ulong_1 (n));     /* { dg-warning "argument 1 value is zero" } */
  sink (f_ulong_2 (1, n));  /* { dg-warning "argument 2 value is zero" } */
}

void
test_long (long n)
{
  sink (f_long_1 (0));      /* { dg-warning "argument 1 value is zero" } */
  sink (f_long_2 (0, 1));   /* { dg-warning "argument 1 value is zero" } */
  sink (f_long_2 (1, 0));   /* { dg-warning "argument 2 value is zero" } */
  sink (f_long_2 (n, 0));   /* { dg-warning "argument 2 value is zero" } */
  sink (f_long_2 (0, n));   /* { dg-warning "argument 1 value is zero" } */

  sink (f_long_1 (n));
  n = 0;
  sink (f_long_1 (n));      /* { dg-warning "argument 1 value is zero" } */
  sink (f_long_2 (1, n));   /* { dg-warning "argument 2 value is zero" } */
}

void
test_size (size_t n)
{
  sink (f_size_1 (0));      /* { dg-warning "argument 1 value is zero" } */
  sink (f_size_2 (0, 1));   /* { dg-warning "argument 1 value is zero" } */
  sink (f_size_2 (1, 0));   /* { dg-warning "argument 2 value is zero" } */
  sink (f_size_2 (n, 0));   /* { dg-warning "argument 2 value is zero" } */
  sink (f_size_2 (0, n));   /* { dg-warning "argument 1 value is zero" } */

  sink (f_size_1 (n));
  n = 0;
  sink (f_size_1 (n));      /* { dg-warning "argument 1 value is zero" } */
  sink (f_size_2 (1, n));   /* { dg-warning "argument 2 value is zero" } */
}

/* Verify that calls to allocation function decorated with attribute
   returns_nonnull don't cause warnings (unlike functions like malloc
   that can return null in this case there's nothing to warn about
   because a returns_nonnull function guarantees success).  */

void
test_size_nonnull (size_t n)
{
  sink (f_size_1_nonnull (0));
  sink (f_size_2_nonnull (0, 1));
  sink (f_size_2_nonnull (1, 0));
  sink (f_size_2_nonnull (n, 0));
  sink (f_size_2_nonnull (0, n));

  sink (f_size_1_nonnull (n));
  n = 0;
  sink (f_size_1_nonnull (n));
  sink (f_size_2_nonnull (1, n));
}

/* Verify that call to plain alloca(0) is not diagnosed.  */

void
test_alloca (size_t n)
{
  extern void* alloca (size_t);

  alloca (0);
}
