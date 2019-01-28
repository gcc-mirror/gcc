/* Exercise that -Warray-bounds is issued for out-of-bounds offsets
   in calls to built-in functions.
   { dg-do compile }
   { dg-options "-O2 -Wno-stringop-overflow -Warray-bounds -ftrack-macro-expansion=0" }  */

#include "../gcc.dg/range.h"

#if __cplusplus
#  define restrict __restrict
extern "C" {
#endif

extern void* memcpy (void* restrict, const void* restrict, size_t);
extern void* mempcpy (void* restrict, const void* restrict, size_t);
extern void* memmove (void*, const void*, size_t);

extern char* stpcpy (char* restrict, const char* restrict);

extern char* strcat (char* restrict, const char* restrict);
extern char* strcpy (char* restrict, const char* restrict);
extern char* strncpy (char* restrict, const char* restrict, size_t);

#if __cplusplus
}   /* extern "C" */
#endif

void sink (void*, ...);

#define CAT(x, y)      x ## y
#define CONCAT(x, y)   CAT (x, y)
#define UNIQUE_NAME(x) CONCAT(x, __LINE__)

#define T(type, N, dst, src, n) do {		\
    extern type UNIQUE_NAME (a)[N];		\
    type *a = UNIQUE_NAME (a);			\
    type *pd = (dst);				\
    const type *ps = (src);			\
    FUNC (pd, ps, n);				\
    sink (a, pd, ps);				\
  } while (0)


void test_memcpy_bounds (char *d, const char *s, size_t n)
{
#define FUNC memcpy

  /* Verify that invalid offsets into an array of known size are
     detected.  */

  T (char, 1, a + SR (DIFF_MIN, -1), s, n);     /* { dg-warning "offset \\\[-\[0-9\]+, -1] is out of the bounds \\\[0, 1] of object \[^\n\r]* with type .char ?\\\[1]" } */
  T (char, 1, a + SR (-2, -1), s, n);     /* { dg-warning "offset \\\[-2, -1] is out of the bounds \\\[0, 1] of object" } */
  T (char, 1, a + SR (-2, 0), s, n);

  T (char, 1, a + UR (0, 1), s, n);
  T (char, 1, a + UR (0, 2), s, n);
  T (char, 1, a + UR (1, 2), s, n);
  T (char, 1, a + UR (2, 3), s, n);       /* { dg-warning "offset \\\[2, 3] is out of the bounds \\\[0, 1] of object " } */
  T (char, 1, a + UR (2, DIFF_MAX), s, n);  /* { dg-warning "offset \\\[2, \[0-9\]+] is out of the bounds \\\[0, 1] of object " "memcpy" } */

  /* Offsets in excess of DIFF_MAX are treated as negative even if
     they appear as large positive in the source.  It would be nice
     if they retained their type but unfortunately that's not how
     it works so be prepared for both in case it even gets fixed.  */
  T (char, 1, a + UR (3, SIZE_MAX - 1), s, n);   /* { dg-warning "offset \\\[3, -?\[0-9\]+] is out of the bounds \\\[0, 1] of object" "memcpy" } */

  /* Verify that invalid offsets into an array of unknown size are
     detected.  */
  extern char arr[];
  T (char, 1, arr + SR (DIFF_MIN,             0), s, n);
  T (char, 1, arr + SR (DIFF_MIN + 1,        -1), s, n);   /* { dg-warning "offset \\\[-\[0-9\]+, -1] is out of the bounds of object " "memcpy" } */
  T (char, 1, arr + SR (DIFF_MIN,             1), s, n);
  T (char, 1, arr + SR (DIFF_MIN,      DIFF_MAX), s, n);
  T (char, 1, arr + SR (       -2,           -1), s, n);   /* { dg-warning "offset \\\[-2, -1] is out of the bounds of object " "memcpy" } */
  T (char, 1, arr + SR (       -1,            0), s, n);
  T (char, 1, arr + SR (       -1,            1), s, n);
  T (char, 1, arr + SR (       -1, DIFF_MAX - 1), s, n);
  T (char, 1, arr + SR (        0,            1), s, n);
  T (char, 1, arr + SR (        0, DIFF_MAX - 1), s, n);
  T (char, 1, arr + SR (        1,            2), s, n);
  T (char, 1, arr + SR (        1, DIFF_MAX - 1), s, n);

  /* Verify that all offsets via a pointer to an uknown object are
     accepted.  */

  /* Negative indices between [DIFF_MIN, DIFF_MAX] are valid since
     the pointer to which the offset is applied can be at a positive
     offset from the beginning of an object.  */
  T (char, 1, d + SR (DIFF_MIN,             0), s, n);
  T (char, 1, d + SR (DIFF_MIN,            -1), s, n);
  T (char, 1, d + SR (DIFF_MIN,             1), s, n);
  T (char, 1, d + SR (DIFF_MIN,  DIFF_MAX - 1), s, n);
  T (char, 1, d + SR (       -2,           -1), s, n);
  T (char, 1, d + SR (       -1,            0), s, n);
  T (char, 1, d + SR (       -1,            1), s, n);
  T (char, 1, d + SR (       -1, DIFF_MAX - 1), s, n);
  T (char, 1, d + SR (        0,            1), s, n);
  T (char, 1, d + SR (        0, DIFF_MAX - 1), s, n);
  T (char, 1, d + SR (        1,            2), s, n);
  T (char, 1, d + SR (        1, DIFF_MAX - 1), s, n);
}

/* Verify offsets in an anti-range.  */

void test_memcpy_bounds_anti_range (char *d, const char *s, size_t n)
{
  T (char, 9, a, a + SAR (-2, -1), 3);
  T (char, 9, a, a + SAR (-1,  1), 3);
  T (char, 9, a, a + SAR ( 0,  1), 3);
  T (char, 9, a, a + SAR ( 0,  2), 3);
  T (char, 9, a, a + SAR ( 0,  3), 3);
  T (char, 9, a, a + SAR ( 0,  4), 3);
  T (char, 9, a, a + SAR ( 0,  5), 3);
  /* The initial source range is valid but the final range after the access
     has complete cannot be.  The value mentioned in the warning is the final
     offset, i.e., 7 + 3.  Including the whole final range because would be
     confusing (the upper bound would either be negative or a very large
     positive number) so only the lower bound is included.  */
  T (char, 9, a, a + SAR ( 0,  6), 3);   /* { dg-warning "forming offset 10 is out of the bounds \\\[0, 9] of object " "memcpy" } */

  /* This fails because the offset isn't represented as an SSA_NAME
     but rather as a GIMPLE_PHI (offset, 0).  With some effort it is
     possible to extract the range from the PHI but it's not implemented
     (yet).  */
  T (char, 9, a, a + SAR ( 1,  6), 3);   /* { dg-warning "forming offset \\\[9, 0] is out of the bounds \\\[0, 9] of object " "memcpy" { xfail *-*-* } } */

  /* The range of offsets is the union of [0, 1] and [7, PTRDIFF_MAX]
     of which the first subrange is valid and thus no warming for memcpy
     is issued.  Similarly for the next test.  */
  T (char, 9, a, a + SAR ( 2,  6), 3);
  T (char, 9, a, a + SAR ( 3,  6), 3);

  T (char, 9, a, a + SAR (-1,  7), 3);   /* { dg-warning "forming offset \\\[10, 11] is out of the bounds \\\[0, 9] of object " "memcpy" } */
  T (char, 9, a, a + SAR (-2,  8), 3);   /* { dg-warning "forming offset \\\[10, 12] is out of the bounds \\\[0, 9] of object " "memcpy" } */
  T (char, 9, a, a + SAR (-3,  7), 5);   /* { dg-warning "forming offset \\\[10, 13] is out of the bounds \\\[0, 9] of object " "memcpy" } */

  T (char, 9, a + SAR (-2, -1), a, 3);
  T (char, 9, a + SAR (-1,  1), a, 3);
  T (char, 9, a + SAR ( 0,  1), a, 3);
  T (char, 9, a + SAR ( 0,  2), a, 3);
  T (char, 9, a + SAR ( 0,  3), a, 3);
  T (char, 9, a + SAR ( 0,  6), a, 3);   /* { dg-warning "forming offset 10 is out of the bounds \\\[0, 9] of object " "memcpy" } */
  T (char, 9, a + SAR (-1,  7), a, 3);   /* { dg-warning "forming offset \\\[10, 11] is out of the bounds \\\[0, 9] of object " "memcpy" } */
  T (char, 9, a + SAR (-2,  8), a, 3);   /* { dg-warning "forming offset \\\[10, 12] is out of the bounds \\\[0, 9] of object " "memcpy" } */

  ptrdiff_t i = SAR (DIFF_MIN + 1, DIFF_MAX - 4);
  T (char, 1, d, d + SAR (DIFF_MIN + 3, DIFF_MAX - 1), 3);
  T (char, 1, d, d + SAR (DIFF_MIN + 3, DIFF_MAX - 3), 5);
}

/* Verify that pointer overflow in the computation done by memcpy
   (i.e., offset + size) is detected and diagnosed.  */

void test_memcpy_overflow (char *d, const char *s, size_t n)
{
  extern char arr[];

  /* Verify that offset overflow involving an array of unknown size
     but known access size is detected.  This works except with small
     sizes that are powers of 2 due to bug .  */
  T (char, 1, arr + SR (DIFF_MAX - 1, DIFF_MAX), s, 1);
  T (char, 1, arr + SR (DIFF_MAX - 1, DIFF_MAX), s, 2);  /* { dg-warning "pointer overflow between offset \\\[\[0-9\]+, \[0-9\]+] and size 2 accessing array " "bug " { xfail non_strict_align } } */
  T (char, 1, arr + SR (DIFF_MAX - 2, DIFF_MAX), s, 3);  /* { dg-warning "pointer overflow between offset \\\[\[0-9\]+, \[0-9\]+] and size 3 accessing array " "memcpy" } */
  T (char, 1, arr + SR (DIFF_MAX - 4, DIFF_MAX), s, 5);  /* { dg-warning "pointer overflow between offset \\\[\[0-9\]+, \[0-9\]+] and size 5 accessing array " "memcpy" } */
}

void test_memcpy_bounds_memarray_range (void)
{
#undef TM
#define TM(mem, dst, src, n)			\
  do {						\
    struct MA { char a5[5]; int i; } ma;	\
    sink (&ma);   /* Initialize arrays.  */	\
    memcpy (dst, src, n);			\
    sink (&ma);					\
  } while (0)

  ptrdiff_t i = SR (1, 2);

  TM (ma.a5, ma.a5 + i, ma.a5, 1);
  TM (ma.a5, ma.a5 + i, ma.a5, 3);
  TM (ma.a5, ma.a5 + i, ma.a5, 5);
  TM (ma.a5, ma.a5 + i, ma.a5, 7);     /* diagnosed with -Warray-bounds=2 */
}

void test_memmove_bounds (char *d, const char *s, size_t n)
{
#undef FUNC
#define FUNC memmove

    T (char, 1, a + SR (DIFF_MIN + 1, -1), s, n);     /* { dg-warning "offset \\\[-\[0-9\]+, -1] is out of the bounds \\\[0, 1] of object \[^\n\r]+ with type .char ?\\\[1]" } */
  T (char, 1, a + SR (-2, -1), s, n);     /* { dg-warning "offset \\\[-2, -1] is out of the bounds \\\[0, 1] of object" } */
  T (char, 1, a + SR (-2, 0), s, n);

  const int *pi = (const int*)s;
  T (int,  2, a + SR (-1, 1), pi, n);
  T (int,  2, a + SR (-1, 2), pi, n);
  T (int,  2, a + SR ( 0, 2), pi, n);
  T (int,  2, a + SR ( 0, 3), pi, n);
  T (int,  2, a + SR ( 1, 3), pi, n);
  T (int,  2, a + SR ( 2, 3), pi, n);

  const int32_t *pi32 = (const int32_t*)s;
  T (int32_t, 2, a + SR ( 3, 4), pi32, n);      /* { dg-warning "offset \\\[12, 16] is out of the bounds \\\[0, 8] of object .\[^\n\r]+. with type .int32_t ?\\\[2]." } */
}


void test_mempcpy_bounds (char *d, const char *s, size_t n)
{
#undef FUNC
#define FUNC mempcpy

  /* Verify that invalid offsets into an array of known size are
     detected.  */

  T (char, 1, a + SR (DIFF_MIN, -1), s, n);     /* { dg-warning "offset \\\[-\[0-9\]+, -1] is out of the bounds"  "mempcpy" } */
T (char, 1, a + SR (-2, -1), s, n);     /* { dg-warning "offset \\\[-2, -1] is out of the bounds"  "mempcpy" } */
  T (char, 1, a + SR (-2, 0), s, n);

  T (char, 1, a + UR (0, 1), s, n);
  T (char, 1, a + UR (0, 2), s, n);
  T (char, 1, a + UR (1, 2), s, n);
  T (char, 1, a + UR (2, 3), s, n);       /* { dg-warning "offset \\\[2, 3] is out of the bounds \\\[0, 1] of object "  "mempcpy" } */
  T (char, 1, a + UR (2, DIFF_MAX), s, n);  /* { dg-warning "offset \\\[2, \[0-9\]+] is out of the bounds \\\[0, 1] of object"  "mempcpy" } */

  /* Offsets in excess of DIFF_MAX are treated as negative even if
     they appear as large positive in the source.  It would be nice
     if they retained their type but unfortunately that's not how
     it works so be prepared for both in case it ever gets fixed.  */
  T (char, 1, a + UR (3, SIZE_MAX), s, n);   /* { dg-warning "offset \\\[3, -?\[0-9\]+] is out of the bounds \\\[0, 1] of object "  "mempcpy" } */

  /* Verify that invalid offsets into an array of unknown size are
     detected.  */
  extern char arr[];
  T (char, 1, arr + SR (DIFF_MIN,         0), s, n);
  T (char, 1, arr + SR (DIFF_MIN,        -1), s, n);   /* { dg-warning "offset \\\[-\[0-9\]+, -1] is out of the bounds of object"  "mempcpy" } */
  T (char, 1, arr + SR (DIFF_MIN,         1), s, n);
  T (char, 1, arr + SR (DIFF_MIN, DIFF_MAX), s, n);
  T (char, 1, arr + SR (       -2,        -1), s, n);   /* { dg-warning "offset \\\[-2, -1] is out of the bounds of object"  "mempcpy" } */
  T (char, 1, arr + SR (       -1,         0), s, n);
  T (char, 1, arr + SR (       -1,         1), s, n);
  T (char, 1, arr + SR (       -1, DIFF_MAX), s, n);
  T (char, 1, arr + SR (        0,         1), s, n);
  T (char, 1, arr + SR (        0, DIFF_MAX), s, n);
  T (char, 1, arr + SR (        1,         2), s, n);
  T (char, 1, arr + SR (        1, DIFF_MAX), s, n);

  /* Verify that all offsets via a pointer to an uknown object are
     accepted.  */

  /* Negative indices between [DIFF_MIN, DIFF_MAX] are valid since
     the pointer to which the offset is applied can be at a positive
     offset from the beginning of an object.  */
  T (char, 1, d + SR (DIFF_MIN,         0), s, n);
  T (char, 1, d + SR (DIFF_MIN,        -1), s, n);
  T (char, 1, d + SR (DIFF_MIN,         1), s, n);
  T (char, 1, d + SR (DIFF_MIN, DIFF_MAX), s, n);
  T (char, 1, d + SR (       -2,        -1), s, n);
  T (char, 1, d + SR (       -1,         0), s, n);
  T (char, 1, d + SR (       -1,         1), s, n);
  T (char, 1, d + SR (       -1, DIFF_MAX), s, n);
  T (char, 1, d + SR (        0,         1), s, n);
  T (char, 1, d + SR (        0, DIFF_MAX), s, n);
  T (char, 1, d + SR (        1,         2), s, n);
  T (char, 1, d + SR (        1, DIFF_MAX), s, n);
}

#define TI(type, N, init, dst, src) do {	\
    type UNIQUE_NAME (a)[N] = init;		\
    type *a = UNIQUE_NAME (a);			\
    type *pd = (dst);				\
    const type *ps = (src);			\
    FUNC (pd, ps);				\
    sink (a, pd, ps, s);			\
  } while (0)

void test_strcpy_bounds (char *d, const char *s)
{
#undef FUNC
#define FUNC strcpy

  ptrdiff_t i;

  TI (char, 1, "",   a, a + SR (DIFF_MIN, 0));
  TI (char, 1, "",   a, a + SR (-1, 0));
  TI (char, 1, "",   a, a + SR (-1, 1));
  TI (char, 1, "",   a, a + SR (0, 1));
  TI (char, 1, "",   a, a + SR (0, DIFF_MAX - 1));
  TI (char, 2, "0",  a, a + SR (0, DIFF_MAX - 1));
  TI (char, 2, "0",  a, a + SR (1, DIFF_MAX - 1));
  /* The following needs a warning for reading past the end.  */
  TI (char, 2, "0",  a, a + SR (2, DIFF_MAX - 1));
  TI (char, 2, "0",  a, a + SR (3, DIFF_MAX - 1));   /* { dg-warning "offset \\\[3, \[0-9\]+] is out of the bounds \\\[0, 2] of object \[^\n\r\]+ with type .char ?\\\[2\\\]."  "strcpy" } */

  TI (char, 3, "01", a, a + SR (0, DIFF_MAX - 1));
  TI (char, 3, "01", a, a + SR (1, DIFF_MAX - 1));
  TI (char, 3, "01", a, a + SR (2, DIFF_MAX - 1));
  /* The following needs a warning for reading past the end.  */
  TI (char, 3, "01", a, a + SR (3, DIFF_MAX - 1));
  TI (char, 3, "01", a, a + SR (4, DIFF_MAX - 1));   /* { dg-warning "offset \\\[4, \[0-9\]+] is out of the bounds \\\[0, 3] of object \[^\n\r\]+ with type .char ?\\\[3\\\]."  "strcpy" } */

  TI (char, 4, "012", a, a + SR (DIFF_MAX - 2, DIFF_MAX - 1));   /* { dg-warning "offset \\\[\[0-9\]+, \[0-9\]+] is out of the bounds \\\[0, 4] of object \[^\n\r\]+ with type .char ?\\\[4\\\]."  "strcpy" } */


  TI (char, 1, "", a + SR (DIFF_MIN, 0), s);
  TI (char, 1, "", a + SR (-1, 0), s);
  TI (char, 1, "", a + SR (-1, 1), s);
  TI (char, 1, "", a + SR (0, 1), s);
  TI (char, 1, "", a + SR (0, DIFF_MAX - 1), s);
  TI (char, 2, "", a + SR (0, DIFF_MAX - 1), s);
  TI (char, 2, "", a + SR (1, DIFF_MAX - 1), s);
  /* The following is diagnosed not because the initial source offset
     it out of bounds (it isn't) but because the final source offset
     after the access has completed, is.  It would be clearer if
     the warning mentioned the final offset.  */
  TI (char, 2, "", a + SR (2, DIFF_MAX - 1), s);   /* { dg-warning "forming offset 3 is out of the bounds \\\[0, 2] of object \[^\n\r\]+ with type .char ?\\\[2\\\]."  "strcpy" } */
  TI (char, 2, "", a + SR (3, DIFF_MAX - 1), s);   /* { dg-warning "offset \\\[3, \[0-9\]+] is out of the bounds \\\[0, 2] of object \[^\n\r\]+ with type .char ?\\\[2\\\]."  "strcpy" } */

  TI (char, 3, "", a + SR (0, DIFF_MAX - 1), s);
  TI (char, 3, "", a + SR (1, DIFF_MAX - 1), s);
  TI (char, 3, "", a + SR (2, DIFF_MAX - 1), s);
  TI (char, 3, "", a + SR (3, DIFF_MAX - 1), s);   /* { dg-warning "forming offset 4 is out of the bounds \\\[0, 3] of object \[^\n\r\]+ with type .char ?\\\[3\\\]."  "strcpy" } */
  TI (char, 3, "", a + SR (4, DIFF_MAX - 1), s);   /* { dg-warning "offset \\\[4, \[0-9\]+] is out of the bounds \\\[0, 3] of object \[^\n\r\]+ with type .char ?\\\[3\\\]."  "strcpy" } */

  TI (char, 4, "", a + SR (DIFF_MAX - 2, DIFF_MAX - 1), s);   /* { dg-warning "offset \\\[\[0-9\]+, \[0-9\]+] is out of the bounds \\\[0, 4] of object \[^\n\r\]+ with type .char ?\\\[4\\\]."  "strcpy" } */
}

struct MA
{
#if __SIZEOF_INT__ == 2
  long i;
#else
  int i;
#endif
  char a5[5];
  char a11[11];
};

struct MA2
{
  struct MA ma3[3];
  struct MA ma5[5];
  char ax[];
};

struct MA3
{
  struct MA2 ma5[3];
  struct MA2 ma7[7];
};

void test_strcpy_bounds_memarray_range (void)
{
#undef TM
#define TM(mem, init, dst, src)			\
  do {						\
    struct MA ma;				\
    strcpy (ma.mem, init);			\
    strcpy (dst, src);				\
    sink (&ma);					\
  } while (0)

  ptrdiff_t i = SR (1, 2);

  TM (a5, "0",    ma.a5 + i, ma.a5);
  TM (a5, "01",   ma.a5 + i, ma.a5);
  TM (a5, "012",  ma.a5 + i, ma.a5);
  TM (a5, "0123", ma.a5 + i, ma.a5);     /* { dg-warning "offset 10 from the object at .ma. is out of the bounds of referenced subobject .\(MA::\)?a5. with type .char ?\\\[5]. at offset 4" "strcpy" } */

  TM (a11, "0",       ma.a5, ma.a11);
  TM (a11, "01",      ma.a5, ma.a11);
  TM (a11, "012",     ma.a5, ma.a11);
  TM (a11, "0123",    ma.a5, ma.a11);
  TM (a11, "01234",   ma.a5, ma.a11);    /* { dg-warning "offset 10 from the object at .ma. is out of the bounds of referenced subobject .\(MA::\)?a5. with type .char ?\\\[5]' at offset 4" } */
  TM (a11, "012345",  ma.a5, ma.a11);    /* { dg-warning "offset \\\[10, 11] from the object at .ma. is out of the bounds of referenced subobject .\(MA::\)?a5. with type .char ?\\\[5]' at offset 4" } */
  TM (a11, "0123456", ma.a5, ma.a11);    /* { dg-warning "offset \\\[10, 12] from the object at .ma. is out of the bounds of referenced subobject .\(MA::\)?a5. with type .char ?\\\[5]' at offset 4" } */

  TM (a11, "0123456", ma.a11 + i, "789abcd");
}

void test_strcpy_bounds_memarray_var (struct MA *pma,
				      struct MA2 *pma2,
				      struct MA3 *pma3,
				      const char *s, size_t n)
{
#undef TM
#define TM(dst, src) do {			\
    strcpy (dst, src);				\
    sink (dst, src);				\
  } while (0)

  TM (pma->a5, s);
  TM (pma->a5 + 0, s);
  TM (pma->a5 + 1, s);
  TM (pma->a5 + 4, s);

  /* The following forms a pointer during the call that's outside
     the bounds of the array it was derived from (pma->a5) so
     it should be diagnosed but the representation of the pointer
     addition doesn't contain information to distinguish it from
     the valid pma->a11 + 1 so this is an XFAIL.  */
  TM (pma->a5 + 5, s);                 /* { dg-warning "offset 17 from the object at .pma. is out of the bounds of .struct MA." "strcpy" { xfail *-*-* } } */

  /* The following also forms an out-of-bounds pointer but similar
     to the above, there is no reliable way to distinguish it from
     (char*)&pma[1].i + 1 so this too is not diagnosed.  */
  TM (pma->a5 + sizeof *pma + 1, s);   /* { dg-warning "offset 17 from the object at .pma. is out of the bounds of .struct MA." "strcpy" { xfail *-*-* } } */

  TM (pma->a5 - 1, s);   /* { dg-warning "offset -1 from the object at .pma. is out of the bounds of .struct MA." "strcpy" { xfail *-*-* } } */

  TM (pma[1].a5, s);
  TM (pma[2].a5 + 0, s);
  TM (pma[3].a5 + 1, s);
  TM (pma[4].a5 + 4, s);


  extern struct MA3 ma3[3];
  TM (ma3[0].ma5[0].ma3[0].a5 + 6, s);
}
