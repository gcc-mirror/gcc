/* Exercise that -Warray-bounds is issued for out-of-bounds offsets
   in calls to built-in functions.
   { dg-do compile }
   { dg-options "-O2 -Warray-bounds=2 -Wno-stringop-overflow -ftrack-macro-expansion=0" }  */

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

struct MA { char a5[5], a7[7]; };

void sink (void*, ...);

void test_memcpy_bounds_memarray_range (void)
{
#undef TM
#define TM(mem, dst, src, n)			\
  do {						\
    struct MA ma;				\
    sink (&ma);   /* Initialize arrays.  */	\
    memcpy (dst, src, n);			\
    sink (&ma);					\
  } while (0)

  ptrdiff_t j = SR (1, 2);

  TM (ma.a5, ma.a5 + j, ma.a5, 1);
  TM (ma.a5, ma.a5 + j, ma.a5, 3);
  TM (ma.a5, ma.a5 + j, ma.a5, 5);
  TM (ma.a5, ma.a5 + j, ma.a5, 7);        /* { dg-warning "offset \\\[5, 7] from the object at .ma. is out of the bounds of referenced subobject .\(MA::\)?a5. with type .char ?\\\[5]. at offset 0" } */
  TM (ma.a5, ma.a5 + j, ma.a5, 9);        /* { dg-warning "offset \\\[5, 9] from the object at .ma. is out of the bounds of referenced subobject .\(MA::\)?a5. with type .char ?\\\[5]. at offset 0" } */
}

#if __i386__ || __x86_64__

/* Disabled for non-x86 targets due to bug 83543.  */

void test_strcpy_bounds_memarray_range (void)
{
#undef TM
#define TM(a5init, a7init, dst, src)		\
  do {						\
    struct MA ma = { a5init, a7init };		\
    strcpy (dst, src);				\
    sink (&ma);					\
  } while (0)

  ptrdiff_t i = SR (1, 2);

  TM ("0", "",     ma.a5 + i, ma.a5);
  TM ("01", "",    ma.a5 + i, ma.a5);
  TM ("012", "",   ma.a5 + i, ma.a5);
  TM ("0123", "",  ma.a5 + i, ma.a5);     /* { dg-warning "offset 5 from the object at .ma. is out of the bounds of referenced subobject .\(MA::\)?a5. with type .char *\\\[5]. at offset 0" "pr83543" { xfail { ! { i?86-*-* x86_64-*-* } } } } */

  TM ("", "012345", ma.a7 + i, ma.a7);    /* { dg-warning "offset 12 from the object at .ma. is out of the bounds of referenced subobject .\(MA::\)?a7. with type .char ?\\\[7]. at offset 5" "pr83543" { xfail { ! { i?86-*-* x86_64-*-* } } } } */
}
#endif
