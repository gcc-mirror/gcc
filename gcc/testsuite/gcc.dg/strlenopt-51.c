/* PR tree-optimization/77357 - strlen of constant strings not folded
   { dg-do compile }
   { dg-options "-O2 -Wall -fdump-tree-gimple -fdump-tree-optimized" } */

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
   scan-tree-dump-time directive at the bottom of the test verifies
   that no such call appears in output.  */
#define ELIM(expr) \
  if (!(expr)) FAIL (in_true_branch_not_eliminated); else (void)0

/* Macro to emit a call to a function named
     call_made_in_{true,false}_branch_on_line_NNN()
   for each call that's expected to be retained.  The dg-final
   scan-tree-dump-time directive at the bottom of the test verifies
   that the expected number of both kinds of calls appears in output
   (a pair for each line with the invocation of the KEEP() macro.  */
#define KEEP(expr)				\
  if (expr)					\
    FAIL (made_in_true_branch);			\
  else						\
    FAIL (made_in_false_branch)

#define T(s, n) ELIM (strlen (s) == n)


struct S
{
  char a1[1], a2[2], a3[3], a4[4], a5[5], a6[6], a7[7], a8[8], a9[9];
};

#define S0 ""
#define S1 "1"
#define S2 "12"
#define S3 "123"
#define S4 "1234"
#define S5 "12345"
#define S6 "123456"
#define S7 "1234567"
#define S8 "12345678"

const char a9[][9] = { S0, S1, S2, S3, S4, S5, S6, S7, S8 };

void test_elim_a9 (int i)
{
  ELIM (strlen (&a9[0][i]) > 0);
  ELIM (strlen (&a9[1][i]) > 1);
  ELIM (strlen (&a9[2][i]) > 2);
  ELIM (strlen (&a9[3][i]) > 3);
  ELIM (strlen (&a9[4][i]) > 4);
  ELIM (strlen (&a9[5][i]) > 5);
  ELIM (strlen (&a9[6][i]) > 6);
  ELIM (strlen (&a9[7][i]) > 7);
  ELIM (strlen (&a9[8][i]) > 8);
}

const char a9_9[][9][9] = {
  { S0, S1, S2, S3, S4, S5, S6, S7, S8 },
  { S1, S2, S3, S4, S5, S6, S7, S8, S0 },
  { S2, S3, S4, S5, S6, S7, S8, S0, S1 },
  { S3, S4, S5, S6, S7, S8, S0, S1, S2 },
  { S4, S5, S6, S7, S8, S0, S1, S2, S3 },
  { S5, S6, S7, S8, S0, S1, S2, S3, S4 },
  { S6, S7, S8, S0, S1, S2, S3, S4, S5 },
  { S7, S8, S0, S1, S2, S3, S4, S5, S6 },
  { S8, S0, S2, S2, S3, S4, S5, S6, S7 }
};

void test_elim_a9_9 (int i)
{
#undef T
#define T(I)					\
  ELIM (strlen (&a9_9[I][0][i]) > (0 + I) % 9);	\
  ELIM (strlen (&a9_9[I][1][i]) > (1 + I) % 9);	\
  ELIM (strlen (&a9_9[I][2][i]) > (2 + i) % 9);	\
  ELIM (strlen (&a9_9[I][3][i]) > (3 + I) % 9);	\
  ELIM (strlen (&a9_9[I][4][i]) > (4 + I) % 9);	\
  ELIM (strlen (&a9_9[I][5][i]) > (5 + I) % 9);	\
  ELIM (strlen (&a9_9[I][6][i]) > (6 + I) % 9);	\
  ELIM (strlen (&a9_9[I][7][i]) > (7 + I) % 9);	\
  ELIM (strlen (&a9_9[I][8][i]) > (8 + I) % 9)

  T (0); T (1); T (2); T (3); T (4); T (5); T (6); T (7); T (8);
}

#line 1000

void test_keep_a9_9 (int i)
{
#undef T
#define T(I)					\
  KEEP (strlen (&a9_9[i][I][0]) > (1 + I) % 9);	\
  KEEP (strlen (&a9_9[i][I][1]) > (1 + I) % 9);	\
  KEEP (strlen (&a9_9[i][I][2]) > (2 + I) % 9);	\
  KEEP (strlen (&a9_9[i][I][3]) > (3 + I) % 9);	\
  KEEP (strlen (&a9_9[i][I][4]) > (4 + I) % 9);	\
  KEEP (strlen (&a9_9[i][I][5]) > (5 + I) % 9);	\
  KEEP (strlen (&a9_9[i][I][6]) > (6 + I) % 9);	\
  KEEP (strlen (&a9_9[i][I][7]) > (7 + I) % 9);	\
  KEEP (strlen (&a9_9[i][I][8]) > (8 + I) % 9)

  T (0); T (1); T (2); T (3); T (4); T (5); T (6); T (7); T (8);
}

/* { dg-final { scan-tree-dump-times "strlen" 72 "gimple" } }
   { dg-final { scan-tree-dump-times "strlen" 63 "optimized" } }

   { dg-final { scan-tree-dump-times "call_made_in_true_branch_on_line_1\[0-9\]\[0-9\]\[0-9\]" 72 "optimized" } }
   { dg-final { scan-tree-dump-times "call_made_in_false_branch_on_line_1\[0-9\]\[0-9\]\[0-9\]" 81 "optimized" } } */
