/* fwrite_unlocked of a single byte whose result is unused writes the same byte
   as fputc_unlocked, so it is folded the same way as the locked form, and into
   the unlocked entry point rather than the locked one.  */
/* { dg-do compile } */
/* { dg-options "-O2 -Wno-unused-result -fdump-tree-optimized" } */

#include <stdio.h>

/* glibc defines fwrite_unlocked as a macro that turns a small constant
   transfer into a putc_unlocked loop, which would keep the call from ever
   reaching the folder under test.  */
#undef fwrite_unlocked

/* Declared here rather than through _GNU_SOURCE so that the test does not
   depend on the host header exposing the unlocked entry points.  */
extern int (fputc_unlocked) (int, FILE *);
extern size_t (fwrite_unlocked) (const void *, size_t, size_t, FILE *);

void one (FILE *f, const char *p) { fwrite_unlocked (p, 1, 1, f); }
size_t used (FILE *f, const char *p) { return fwrite_unlocked (p, 1, 1, f); }
void two_items (FILE *f, const char *p) { fwrite_unlocked (p, 1, 2, f); }
void two_bytes (FILE *f, const char *p) { fwrite_unlocked (p, 2, 1, f); }
void unknown (FILE *f, const char *p, size_t n) { fwrite_unlocked (p, 1, n, f); }

/* { dg-final { scan-tree-dump-times "fputc_unlocked" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "fwrite_unlocked" 4 "optimized" } } */
/* The unlocked form must not be folded into the locked fputc.  A call with no
   result is dumped as "  fputc (...", which the leading space matches without
   also matching fputc_unlocked.  */
/* { dg-final { scan-tree-dump-not " fputc \\(" "optimized" } } */
