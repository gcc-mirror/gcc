/* PR tree-optimization/91996 - fold strlen relational expressions

   The optimization is only implemented for MEM_REF stores and other
   targets than those below may not transform the memcpy call into
   such a store.
   { dg-do compile { target aarch64*-*-* i?86-*-* powerpc*-*-* x86_64-*-* } }

   { dg-options "-O2 -Wall -fdump-tree-optimized" } */

#define CHAR_BIT      __CHAR_BIT__
#define SIZE_MAX      __SIZE_MAX__
#define LEN_MAX       (__PTRDIFF_MAX__ - 2)

typedef __PTRDIFF_TYPE__ ptrdiff_t;
typedef __SIZE_TYPE__    size_t;

extern void* memcpy (void*, const void*, size_t);
extern size_t strlen (const char*);

#define CONCAT(a, b) a ## b
#define CAT(a, b)    CONCAT (a, b)

extern void sink (void*, ...);
extern void failure_on_line (int);

extern char src[];
extern char dst[];

/* Copy (1 << NCPYLOG) bytes from an unknown string SRC with strlen (SRC)
   in the range [MINSRCLEN, MAXSRCLEN] into DST + DSTOFF and verify that
   strlen (DST + DSTOFF) is in the range [MINDSTLEN, MAXDSTLEN].  */
#define MIN_MAX(dst, dstoff, src,					\
	     minsrclen, maxsrclen, mindstlen, maxdstlen, ncpylog)	\
  void CAT (test_on_line_, __LINE__) (void)				\
  {									\
    size_t srclen = strlen (src);					\
    if ((minsrclen) <= srclen && srclen <= (maxsrclen)) {		\
      char *d = (dst) + (dstoff);					\
      memcpy (d, src, (size_t)1 << (ncpylog));				\
      size_t dstlen = strlen (d);					\
      if (dstlen < (mindstlen) || (maxdstlen) < dstlen)			\
	{								\
	  failure_on_line (__LINE__);					\
	}								\
      sink (dst, src);							\
    }									\
  } typedef void dummy_type

// Verify the lower bound of the resulting strlen range.
#define MIN(dst, dstoff, src, minsrclen, mindstlen, ncpylog)		\
  MIN_MAX (dst, dstoff, src, minsrclen, LEN_MAX, mindstlen, LEN_MAX, ncpylog)

MIN (dst, 0, src, 2, 1, 0);
MIN (dst, 0, src, 3, 1, 0);
MIN (dst, 0, src, 3, 2, 1);
MIN (dst, 0, src, 3, 2, 2);
MIN (dst, 0, src, 3, 2, 3);

MIN (dst, 1, src, 2, 1, 0);
MIN (dst, 1, src, 3, 1, 0);
MIN (dst, 1, src, 3, 2, 1);
MIN (dst, 1, src, 3, 2, 2);
MIN (dst, 1, src, 3, 2, 3);

MIN (dst, 2, src, 2, 1, 0);
MIN (dst, 3, src, 3, 1, 0);
MIN (dst, 4, src, 3, 2, 1);
MIN (dst, 5, src, 3, 2, 2);
MIN (dst, 6, src, 3, 2, 3);


MIN (dst, 0, src, 5, 1, 0);
MIN (dst, 0, src, 5, 2, 1);
MIN (dst, 0, src, 5, 4, 2);
MIN (dst, 0, src, 5, 5, 3);

#if __aarch64__ || __x86_64__
/* Of the targets above only aarch64 and x86_64 transform memcpy calls
   of (2 << 4) bytes into MEM_REF.  */
MIN (dst, 0, src, 5, 5, 4);
#endif

MIN (dst, 11, src, 5, 1, 0);
MIN (dst, 22, src, 5, 2, 1);
MIN (dst, 33, src, 5, 4, 2);
MIN (dst, 44, src, 5, 5, 3);

#if __aarch64__ || __x86_64__
MIN (dst, 55, src, 5, 5, 4);
#endif

MIN (dst, 11, src, LEN_MAX, 1, 0);
MIN (dst, 22, src, LEN_MAX, 2, 1);
MIN (dst, 33, src, LEN_MAX, 4, 2);
MIN (dst, 44, src, LEN_MAX, 5, 3);
MIN (dst, 55, src, LEN_MAX, 5, 4);
MIN (dst, 66, src, LEN_MAX, 9, 8);
MIN (dst, 66, src, LEN_MAX, LEN_MAX, sizeof (ptrdiff_t) * CHAR_BIT - 1);


MIN_MAX (dst, 0, src, 3, 5, 1, LEN_MAX, 0);
MIN_MAX (dst, 0, src, 3, 5, 2, LEN_MAX, 1);
MIN_MAX (dst, 0, src, 3, 5, 3, LEN_MAX, 2);

/* Upper bound not implemented yet.
   MIN_MAX (dst, 0, src, 3, 5, 3, 5, 3);  */

/* { dg-final { scan-tree-dump-times "failure_on_line \\(" 0 "optimized" } } */
