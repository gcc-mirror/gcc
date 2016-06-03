/* Test rint and related functions expanded inline for SSE2.  All
   should be expanded when spurious "inexact" allowed.  */
/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -mfpmath=sse -ffp-int-builtin-inexact" } */
/* { dg-add-options c99_runtime } */
/* { dg-require-effective-target sse2 } */

#define TEST(FN, TYPE)				\
  do						\
    {						\
      volatile TYPE a = 1.5, b;			\
      b = FN (a);				\
    }						\
  while (0)

#define FN_TESTS(FN)					\
  do							\
    {							\
      TEST (__builtin_ ## FN, double);			\
      TEST (__builtin_ ## FN ## f, float);		\
    }							\
  while (0)

void
test (void)
{
  FN_TESTS (rint);
  FN_TESTS (ceil);
  FN_TESTS (floor);
  FN_TESTS (trunc);
}

/* { dg-final { scan-assembler-not "\[ \t\]rint" } } */
/* { dg-final { scan-assembler-not "\[ \t\]ceil" } } */
/* { dg-final { scan-assembler-not "\[ \t\]floor" } } */
/* { dg-final { scan-assembler-not "\[ \t\]trunc" } } */
