/* PR tree-optimization/86083 - handle non-constant assignments in strlen
   { dg-do compile }
   { dg-options "-O2 -Wall -fdump-tree-optimized" } */

#include "range.h"
#include "strlenopt.h"

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
#define ASSERT_ELIM(expr)						\
  if (!(expr)) FAIL (in_true_branch_not_eliminated); else (void)0

/* Macro to emit a call to a function named
     call_made_in_{true,false}_branch_on_line_NNN()
   for each call that's expected to be retained.  The dg-final
   scan-tree-dump-times directive at the bottom of the test verifies
   that the expected number of both kinds of calls appears in output
   (a pair for each line with the invocation of the KEEP() macro.  */
#define ASSERT_KEEP(expr)			\
  if (expr)					\
    FAIL (made_in_true_branch);			\
  else						\
    FAIL (made_in_false_branch)


#define ELIM(init, i, c, res)			\
  do {						\
    char a[] = init;				\
    a[i] = c;					\
    ASSERT_ELIM (strlen (a) == res);		\
  } while (0)

#define KEEP(init, i, c, res)			\
  do {						\
    char a[] = init;				\
    a[i] = c;					\
    ASSERT_KEEP (strlen (a) == res);		\
  } while (0)


void test_elim_range (char c)
{
  ELIM ("1", 0, UR (1, 2), 1);
  ELIM ("1", 0, UR (1, 127), 1);
  ELIM ("1", 0, UR ('0', '9'), 1);

  ELIM ("12", 0, UR (1, 127), 2);
  ELIM ("12", 1, UR (1, 127), 2);

  ELIM ("123", 0, UR (1, 9), 3);
  ELIM ("123", 1, UR (10, 99), 3);
  ELIM ("123", 2, UR (100, 127), 3);
}

void test_elim_anti_range (const char *s)
{
  char c = *s++;
  ELIM ("123", 0, c ? c : 'x', 3);

  c = *s++;
  ELIM ("1234", 1, c ? c : 'y', 4);

  c = *s++;
  ELIM ("123", 2, c ? c : 'z', 3);
}

#line 1000

void test_keep (void)
{
  size_t uchar_max = (unsigned char)-1;

  KEEP ("1",     0, UR (1, uchar_max + 1), 1);
  KEEP ("1\0\3", 1, UR (1, 2), 2);
}

/* { dg-final { scan-tree-dump-times "call_in_true_branch_not_eliminated_" 0 "optimized" } }

   { dg-final { scan-tree-dump-times "call_made_in_true_branch_on_line_1\[0-9\]\[0-9\]\[0-9\]" 2 "optimized" } }
   { dg-final { scan-tree-dump-times "call_made_in_false_branch_on_line_1\[0-9\]\[0-9\]\[0-9\]" 2 "optimized" } } */
