/* fwrite of a single byte whose result is unused writes the same byte as
   fputc, so it should be folded.  */
/* { dg-do compile } */
/* { dg-options "-O2 -Wno-unused-result -fdump-tree-optimized" } */

#include <stdio.h>

void one (FILE *f, const char *p) { fwrite (p, 1, 1, f); }
size_t used (FILE *f, const char *p) { return fwrite (p, 1, 1, f); }
void two_items (FILE *f, const char *p) { fwrite (p, 1, 2, f); }
void two_bytes (FILE *f, const char *p) { fwrite (p, 2, 1, f); }
void unknown (FILE *f, const char *p, size_t n) { fwrite (p, 1, n, f); }

/* { dg-final { scan-tree-dump-times "fputc" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "fwrite" 4 "optimized" } } */
