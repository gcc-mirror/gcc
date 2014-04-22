/* { dg-options "-O -fdump-tree-optimized" } */

extern void *memcpy (void *, const void *, __SIZE_TYPE__);

#define TEST(NAME, TYPE) \
  TYPE NAME##x; \
  char NAME##y[sizeof (NAME##x)] __attribute__((aligned (__alignof__ (NAME##x)))); \
  void NAME (void) { memcpy (&NAME##x, &NAME##y, sizeof (NAME##x)); }

TEST (f, float);
TEST (d, double);
TEST (ld, long double);
TEST (cf, _Complex float);
TEST (cd, _Complex double);
TEST (cld, _Complex long double);
TEST (d8f, float __attribute__((vector_size (8))));
TEST (d16f, float __attribute__((vector_size (16))));
TEST (d32f, float __attribute__((vector_size (32))));
TEST (d64f, float __attribute__((vector_size (64))));
TEST (d128f, float __attribute__((vector_size (128))));
TEST (d16d, double __attribute__((vector_size (16))));
TEST (d32d, double __attribute__((vector_size (32))));
TEST (d64d, double __attribute__((vector_size (64))));
TEST (d128d, double __attribute__((vector_size (128))));

/* { dg-final { scan-tree-dump-not "memcpy" "optimized" { target x86_64-*-* } } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
