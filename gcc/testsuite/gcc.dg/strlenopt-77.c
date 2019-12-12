/* PR tree-optimization/91315 - missing strlen lower bound of a string
   known to be at least N characters
   { dg-do compile }
   { dg-options "-O2 -Wall -fdump-tree-optimized" } */

#include "strlenopt.h"

#define CAT(x, y) x ## y
#define CONCAT(x, y) CAT (x, y)
#define FAILNAME(name) CONCAT (call_ ## name ##_on_line_, __LINE__)

#define FAIL(name) do {				\
    extern void FAILNAME (name) (void);		\
    FAILNAME (name)();				\
  } while (0)

/* Macro to emit a call to function named
     call_in_true_branch_not_eliminated_on_line_NNN()
   for each call that's expected to be eliminated.  The dg-final
   scan-tree-dump-time directive at the bottom of the test verifies
   that no such call appears in output.  */
#define ASSERT_ELIM(expr)						\
  if (!!(expr)) FAIL (in_true_branch_not_eliminated); else (void)0

char a[32];

void lower_bound_assign_1 (void)
{
  a[0] = '1';
  ASSERT_ELIM (strlen (a) < 1);
}

void lower_bound_assign_2 (void)
{
  a[0] = '1';
  a[1] = '2';
  ASSERT_ELIM (strlen (a) < 2);
}

void lower_bound_assign_3 (void)
{
  a[0] = '1';
  a[1] = '2';
  a[2] = '3';
  ASSERT_ELIM (strlen (a) < 3);
}

void lower_bound_memcpy (void)
{
  memcpy (a, "123", 3);
  ASSERT_ELIM (strlen (a) < 3);
}

void lower_bound_memcpy_memcpy_2 (void)
{
  memcpy (a, "123", 3);
  memcpy (a + 2, "345", 3);
  ASSERT_ELIM (strlen (a) < 5);
}

void lower_bound_memcpy_memcpy_3 (void)
{
  memcpy (a, "123", 3);
  memcpy (a + 3, "456", 3);
  ASSERT_ELIM (strlen (a) < 6);
}

/* FIXME: Not optimized yet.
void lower_bound_stpcpy_stpcpy_assign (void)
{
  *stpcpy (strcpy (a, "123"), "4567") = '8';
  ASSERT_ELIM (strlen (a) < 8);
}
*/

void lower_bound_strcpy_strcat_assign (void)
{
  strcpy (a, "123");
  strcat (a, "45");
  a[5] = '6';
  ASSERT_ELIM (strlen (a) < 6);
}

/* { dg-final { scan-tree-dump-times "call_in_true_branch_not_eliminated_" 0 "optimized" } } */
