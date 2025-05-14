/* PR tree-optimization/86415 - strlen() not folded for substrings
   within constant arrays
   { dg-do compile }
   { dg-options "-O2 -Wall -fdump-tree-gimple -fdump-tree-ccp" } */

#include "strlenopt.h"

#define CONCAT(x, y) x ## y
#define CAT(x, y) CONCAT (x, y)
#define FAILNAME(name) CAT (call_ ## name ##_on_line_, __LINE__)

#define FAIL(name) do {				\
    extern void FAILNAME (name) (void);		\
    FAILNAME (name)();				\
  } while (0)

/* Macro to emit a call to funcation named
     call_in_true_branch_not_eliminated_on_line_NNN()
   for each call that's expected to be eliminated.  The dg-final
   scan-tree-dump-times directive at the bottom of the test verifies
   that no such call appears in output.  */
#define ELIM(expr) \
  if (!(expr)) FAIL (in_true_branch_not_eliminated); else (void)0

#define T(s, n) ELIM (strlen (s) == n)

/*                              11111
	     0 1  23 4  567 8  901234  */
#define STR "1\00012\000123\0001234\0"

const char a[]   = STR;
const char b[20] = STR;

void test_literal (void)
{
  /* Verify that strlen() of substrings within a string literal are
     correctly folded.  */
  T (STR,      1);  T (STR +  1, 0);  T (STR +  2, 2);  T (STR +  3, 1);
  T (STR +  4, 0);  T (STR +  5, 3);  T (STR +  6, 2);  T (STR +  7, 1);
  T (STR +  8, 0);  T (STR +  9, 4);  T (STR + 10, 3);  T (STR + 11, 2);
  T (STR + 12, 1);  T (STR + 13, 0);  T (STR + 14, 0);

  T (&(STR[0]),  1);  T (&(STR[ 1]), 0);  T (&(STR[ 2]), 2);
  T (&(STR[ 3]), 1);  T (&(STR[ 4]), 0);  T (&(STR[ 5]), 3);
  T (&(STR[ 6]), 2);  T (&(STR[ 7]), 1);  T (&(STR[ 8]), 0);
  T (&(STR[ 9]), 4);  T (&(STR[10]), 3);  T (&(STR[11]), 2);
  T (&(STR[12]), 1);  T (&(STR[13]), 0);  T (&(STR[14]), 0);

  T (&(STR[0])  +  1, 0);  T (&(STR[ 1]) +  1, 2);  T (&(STR[ 2]) +  1, 1);
  T (&(STR[ 3]) +  1, 0);  T (&(STR[ 4]) +  1, 3);  T (&(STR[ 5]) +  1, 2);
  T (&(STR[ 6]) +  1, 1);  T (&(STR[ 7]) +  1, 0);  T (&(STR[ 8]) +  1, 4);
  T (&(STR[ 9]) +  1, 3);  T (&(STR[10]) +  1, 2);  T (&(STR[11]) +  1, 1);
  T (&(STR[12]) +  1, 0);  T (&(STR[13]) +  1, 0);  T (&(STR[13]) - 13, 1);
  T (&(STR[13]) - 12, 0);  T (&(STR[13]) - 11, 2);  T (&(STR[13]) - 10, 1);
}

void test_array (void)
{
  /* Verify that strlen() of substrings within a fully initialized
     array are correctly folded.  */
  T (a,      1);  T (a +  1, 0);  T (a +  2, 2);  T (a +  3, 1);
  T (a +  4, 0);  T (a +  5, 3);  T (a +  6, 2);  T (a +  7, 1);
  T (a +  8, 0);  T (a +  9, 4);  T (a + 10, 3);  T (a + 11, 2);
  T (a + 12, 1);  T (a + 13, 0);  T (a + 14, 0);

  /* Verify that strlen() of substrings within a partially initialized
     array are also correctly folded, including those referring to
     the empty substrings in the implicitly initialized elements.  */
  T (b,      1);  T (b +  1, 0);  T (b +  2, 2);  T (b +  3, 1);
  T (b +  4, 0);  T (b +  5, 3);  T (b +  6, 2);  T (b +  7, 1);
  T (b +  8, 0);  T (b +  9, 4);  T (b + 10, 3);  T (b + 11, 2);
  T (b + 12, 1);  T (b + 13, 0);  T (b + 14, 0);  T (b + 15, 0);
  T (b + 16, 0);  T (b + 17, 0);  T (b + 18, 0);  T (b + 19, 0);
}

void test_array_ref_plus (void)
{
  /* Verify that strlen() of substrings within a fully initialized
     array referred to by array indices with offsets are correctly
     folded.  */
  T (&a[ 0],     1);  T (&a[ 0] + 1, 0);
  T (&a[ 1],     0);  T (&a[ 1] + 1, 2);
  T (&a[ 2],     2);  T (&a[ 2] + 1, 1);  T (&a[ 2] + 2, 0);
  T (&a[ 3],     1);  T (&a[ 3] + 1, 0);
  T (&a[ 4],     0);  T (&a[ 4] + 1, 3);
  T (&a[ 5],     3);  T (&a[ 5] + 1, 2);
  T (&a[ 5] + 2, 1);  T (&a[ 5] + 3, 0);  T (&a[ 5] + 4, 4);
  T (&a[ 6],     2);  T (&a[ 6] + 1, 1);  T (&a[ 6] + 2, 0);
  T (&a[ 7],     1);  T (&a[ 7] + 1, 0);
  T (&a[ 8],     0);  T (&a[ 8] + 1, 4);
  T (&a[ 9],     4);  T (&a[ 9] + 1, 3);  T (&a[ 9] + 2, 2);
  T (&a[ 9] + 3, 1);  T (&a[ 9] + 4, 0);  T (&a[ 9] + 5, 0);
  T (&a[10],     3);  T (&a[10] + 1, 2);  T (&a[10] + 2, 1);
  T (&a[10] + 3, 0);  T (&a[10] + 4, 0);
  T (&a[11],     2);  T (&a[11] + 1, 1);  T (&a[11] + 2, 0);
  T (&a[12],     1);  T (&a[12] + 1, 0);  T (&a[12] + 2, 0);
  T (&a[13],     0);  T (&a[13] + 1, 0);
  T (&a[14],     0);
}

void test_array_ref (void)
{
  T (&a[ 0], 1);  T (&a[ 1], 0);  T (&a[ 2], 2);  T (&a[ 3], 1);
  T (&a[ 4], 0);  T (&a[ 5], 3);  T (&a[ 6], 2);  T (&a[ 7], 1);
  T (&a[ 8], 0);  T (&a[ 9], 4);  T (&a[10], 3);  T (&a[11], 2);
  T (&a[12], 1);  T (&a[13], 0);  T (&a[14], 0);

  T (&b[ 0], 1);  T (&b[ 1], 0);  T (&b[ 2], 2);  T (&b[ 3], 1);
  T (&b[ 4], 0);  T (&b[ 5], 3);  T (&b[ 6], 2);  T (&b[ 7], 1);
  T (&b[ 8], 0);  T (&b[ 9], 4);  T (&b[10], 3);  T (&b[11], 2);
  T (&b[12], 1);  T (&b[13], 0);  T (&b[14], 0);  T (&b[15], 0);
  T (&b[16], 0);  T (&b[17], 0);  T (&b[18], 0);  T (&b[19], 0);
}

/* { dg-final { scan-tree-dump-times "strlen1" 0 "gimple" } }
   { dg-final { scan-tree-dump-times "call_in_true_branch_not_eliminated" 0 "ccp1" } } */
