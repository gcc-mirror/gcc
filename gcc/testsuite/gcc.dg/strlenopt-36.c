/* PR tree-optimization/78450 - strlen(s) return value can be assumed
   to be less than the size of s
   { dg-do compile }
   { dg-options "-O2 -fdump-tree-optimized" } */

#include "strlenopt.h"

extern char a7[7], a6[6], a5[5], a4[4], a3[3], a2[2], a1[1];
extern char a0[0];   /* Intentionally not tested here.  */
extern char ax[];    /* Same.  */

extern void failure_on_line (int);

#define TEST_FAIL(line)					\
  do {							\
    failure_on_line (line);				\
  } while (0)

#define T(expr)						\
  if (!(expr)) TEST_FAIL (__LINE__); else (void)0


void test_array (void)
{
  T (strlen (a7) < sizeof a7);
  T (strlen (a6) < sizeof a6);
  T (strlen (a5) < sizeof a5);
  T (strlen (a4) < sizeof a4);
  T (strlen (a3) < sizeof a3);

  /* The following two calls are folded too early which defeats
     the strlen() optimization.
    T (strlen (a2) == 1);
    T (strlen (a1) == 0);  */
}

/* { dg-final { scan-tree-dump-not "failure_on_line" "optimized" } } */
