/* PR tree-optimization/81384 - built-in form of strnlen missing
   Test to verify that strnlen built-in expansion works correctly
   in the absence of tree strlen optimization.
   { dg-do compile }
   { dg-options "-O2 -Wall -Wno-stringop-overflow -fdump-tree-optimized" } */

#include "strlenopt.h"

#define PTRDIFF_MAX __PTRDIFF_MAX__
#define SIZE_MAX    __SIZE_MAX__

typedef __SIZE_TYPE__ size_t;

extern void abort (void);
extern size_t strnlen (const char *, size_t);

#define CAT(x, y) x ## y
#define CONCAT(x, y) CAT (x, y)
#define FAILNAME(name) CONCAT (call_ ## name ##_on_line_, __LINE__)

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

/* Macro to emit a call to a function named
     call_made_in_{true,false}_branch_on_line_NNN()
   for each call that's expected to be retained.  The dg-final
   scan-tree-dump-times directive at the bottom of the test verifies
   that the expected number of both kinds of calls appears in output
   (a pair for each line with the invocation of the KEEP() macro.  */
#define KEEP(expr)				\
  if (expr)					\
    FAIL (made_in_true_branch);			\
  else						\
    FAIL (made_in_false_branch)

extern char c;
extern char a1[1];
extern char a3[3];
extern char a5[5];
extern char a3_7[3][7];
extern char ax[];

void elim_strnlen_arr_cst (void)
{
  /* The length of a string stored in a one-element array must be zero.
     The result reported by strnlen() for such an array can be non-zero
     only when the bound is equal to 1 (in which case the result must
     be one).  */
  ELIM (strnlen (&c, 0) == 0);
  ELIM (strnlen (&c, 1) < 2);
  ELIM (strnlen (&c, 2) == 0);
  ELIM (strnlen (&c, 9) == 0);
  ELIM (strnlen (&c, PTRDIFF_MAX) == 0);
  ELIM (strnlen (&c, SIZE_MAX) == 0);
  ELIM (strnlen (&c, -1) == 0);

  ELIM (strnlen (a1, 0) == 0);
  ELIM (strnlen (a1, 1) < 2);
  ELIM (strnlen (a1, 2) == 0);
  ELIM (strnlen (a1, 9) == 0);
  ELIM (strnlen (a1, PTRDIFF_MAX) == 0);
  ELIM (strnlen (a1, SIZE_MAX) == 0);
  ELIM (strnlen (a1, -1) == 0);

  ELIM (strnlen (a3, 0) == 0);
  ELIM (strnlen (a3, 1) < 2);
  ELIM (strnlen (a3, 2) < 3);
  ELIM (strnlen (a3, 3) < 4);
  ELIM (strnlen (a3, 9) < 4);
  ELIM (strnlen (a3, PTRDIFF_MAX) < 4);
  ELIM (strnlen (a3, SIZE_MAX) < 4);
  ELIM (strnlen (a3, -1) < 4);

  ELIM (strnlen (a3_7[0], 0) == 0);
  ELIM (strnlen (a3_7[0], 1) < 2);
  ELIM (strnlen (a3_7[0], 2) < 3);
  ELIM (strnlen (a3_7[0], 3) < 4);
  ELIM (strnlen (a3_7[0], 9) <= 9);
  ELIM (strnlen (a3_7[0], PTRDIFF_MAX) <= sizeof a3_7);
  ELIM (strnlen (a3_7[0], SIZE_MAX) <= sizeof a3_7);
  ELIM (strnlen (a3_7[0], -1) <= sizeof a3_7);

  ELIM (strnlen (a3_7[2], 0) == 0);
  ELIM (strnlen (a3_7[2], 1) < 2);
  ELIM (strnlen (a3_7[2], 2) < 3);
  ELIM (strnlen (a3_7[2], 3) < 4);
  ELIM (strnlen (a3_7[2], 9) <= 9);
  ELIM (strnlen (a3_7[2], PTRDIFF_MAX) < sizeof a3_7);
  ELIM (strnlen (a3_7[2], SIZE_MAX) < sizeof a3_7);
  ELIM (strnlen (a3_7[2], -1) < sizeof a3_7);

  ELIM (strnlen ((char*)a3_7, 0) == 0);
  ELIM (strnlen ((char*)a3_7, 1) < 2);
  ELIM (strnlen ((char*)a3_7, 2) < 3);
  ELIM (strnlen ((char*)a3_7, 3) < 4);
  ELIM (strnlen ((char*)a3_7, 9) < 10);
  ELIM (strnlen ((char*)a3_7, 19) < 20);
  ELIM (strnlen ((char*)a3_7, 21) <= sizeof a3_7);
  ELIM (strnlen ((char*)a3_7, 23) <= sizeof a3_7);
  ELIM (strnlen ((char*)a3_7, PTRDIFF_MAX) <= sizeof a3_7);
  ELIM (strnlen ((char*)a3_7, SIZE_MAX) <= sizeof a3_7);
  ELIM (strnlen ((char*)a3_7, -1) <= sizeof a3_7);

  ELIM (strnlen (ax, 0) == 0);
  ELIM (strnlen (ax, 1) < 2);
  ELIM (strnlen (ax, 2) < 3);
  ELIM (strnlen (ax, 9) < 10);
  ELIM (strnlen (ax, PTRDIFF_MAX) < PTRDIFF_MAX);
  ELIM (strnlen (ax, SIZE_MAX) < PTRDIFF_MAX);
  ELIM (strnlen (ax, -1) < PTRDIFF_MAX);
}


void elim_strnlen_str_cst (void)
{
  const char *s0 = "";
  const char *s1 = "1";
  const char *s3 = "123";

  ELIM (strnlen (s0, 0) == 0);
  ELIM (strnlen (s0, 1) == 0);
  ELIM (strnlen (s0, 9) == 0);
  ELIM (strnlen (s0, PTRDIFF_MAX) == 0);
  ELIM (strnlen (s0, SIZE_MAX) == 0);
  ELIM (strnlen (s0, -1) == 0);

  ELIM (strnlen (s1, 0) == 0);
  ELIM (strnlen (s1, 1) == 1);
  ELIM (strnlen (s1, 9) == 1);
  ELIM (strnlen (s1, PTRDIFF_MAX) == 1);
  ELIM (strnlen (s1, SIZE_MAX) == 1);
  ELIM (strnlen (s1, -2) == 1);

  ELIM (strnlen (s3, 0) == 0);
  ELIM (strnlen (s3, 1) == 1);
  ELIM (strnlen (s3, 2) == 2);
  ELIM (strnlen (s3, 3) == 3);
  ELIM (strnlen (s3, 9) == 3);
  ELIM (strnlen (s3, PTRDIFF_MAX) == 3);
  ELIM (strnlen (s3, SIZE_MAX) == 3);
  ELIM (strnlen (s3, -2) == 3);
}

void elim_strnlen_range (char *s)
{
  const char *s0 = "";
  const char *s1 = "1";
  const char *s3 = "123";

  size_t n_0_1 = (size_t)s & 1;
  size_t n_0_2 = ((size_t)s & 3) < 3 ? ((size_t)s & 3) : 2;
  size_t n_0_3 = (size_t)s & 3;
  size_t n_1_2 = n_0_1 + 1;

  ELIM (strnlen (s0, n_0_1) == 0);
  ELIM (strnlen (s0, n_0_2) == 0);
  ELIM (strnlen (s0, n_1_2) == 0);

  ELIM (strnlen (s1, n_0_1) < 2);
  ELIM (strnlen (s1, n_0_2) < 2);
  ELIM (strnlen (s1, n_0_3) < 2);

  ELIM (strnlen (s1, n_1_2) > 0);
  ELIM (strnlen (s1, n_1_2) < 2);

  ELIM (strnlen (s3, n_0_1) < 2);
  ELIM (strnlen (s3, n_0_2) < 3);
  ELIM (strnlen (s3, n_0_3) < 4);

  ELIM (strnlen (s3, n_1_2) > 0);
  ELIM (strnlen (s3, n_1_2) < 4);
}


#line 1000

void keep_strnlen_arr_cst (void)
{
  KEEP (strnlen (&c, 1) == 0);
  KEEP (strnlen (&c, 1) == 1);

  KEEP (strnlen (a1, 1) == 0);
  KEEP (strnlen (a1, 1) == 1);

  KEEP (strnlen (ax, 9) < 9);
}

struct FlexArrays
{
  char c;
  char a0[0];   /* Access to internal zero-length arrays are undefined.  */
  char a1[1];
};

void keep_strnlen_memarr_cst (struct FlexArrays *p)
{
  KEEP (strnlen (&p->c, 1) == 0);
  KEEP (strnlen (&p->c, 1) == 1);

#if 0
  /* Accesses to internal zero-length arrays are undefined so avoid
     exercising them.  */
  KEEP (strnlen (p->a0, 1) == 0);
  KEEP (strnlen (p->a0, 1) == 1);
  KEEP (strnlen (p->a0, 9) < 9);
#endif

  KEEP (strnlen (p->a1, 1) == 0);
  KEEP (strnlen (p->a1, 1) == 1);

  KEEP (strnlen (p->a1, 2) == 0);
  KEEP (strnlen (p->a1, 2) == 1);
  KEEP (strnlen (p->a1, 2) == 2);

  KEEP (strnlen (p->a1, 9) < 9);
}

/* { dg-final { scan-tree-dump-times "call_in_true_branch_not_eliminated_" 0 "optimized" } }

   { dg-final { scan-tree-dump-times "call_made_in_true_branch_on_line_1\[0-9\]\[0-9\]\[0-9\]" 13 "optimized" } }
   { dg-final { scan-tree-dump-times "call_made_in_false_branch_on_line_1\[0-9\]\[0-9\]\[0-9\]" 13 "optimized" } } */
