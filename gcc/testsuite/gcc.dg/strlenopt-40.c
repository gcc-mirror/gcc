/* PR tree-optimization/83671 - fix for false positive reported by
   -Wstringop-overflow does not work with inlining
   { dg-do compile }
   { dg-options "-O1 -fdump-tree-optimized" } */

#include "strlenopt.h"

#define DIFF_MAX __PTRDIFF_MAX__

#define CAT(x, y) x ## y
#define CONCAT(x, y) CAT (x, y)
#define FAILNAME(name) CONCAT (call_ ## name ##_on_line_, __LINE__)

#define FAIL(name) do {				\
    extern void FAILNAME (name) (void);		\
    FAILNAME (name)();				\
  } while (0)

/* Macros to emit a call to funcation named
     call_in_{true,false}_branch_not_eliminated_on_line_NNN()
   for each call that's expected to be eliminated.  The dg-final
   scan-tree-dump-time directive at the bottom of the test verifies
   that no such call appears in output.  */
#define ELIM_TRUE(expr) \
  if (!(expr)) FAIL (in_true_branch_not_eliminated); else (void)0

#define ELIM_FALSE(expr)					\
  if (!!(expr)) FAIL (in_false_branch_not_eliminated); else (void)0

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

typedef char A3[3], A5[5], A7[7], AX[];

typedef A3 A7_3[7];
typedef A3 AX_3[];
typedef A5 A7_5[7];
typedef A7 A5_7[5];

extern A7_3 a7_3;
extern A5_7 a5_7;
extern AX_3 ax_3;

extern A3 a3;
extern A7 a5;
extern A7 a7;
extern AX ax;

extern A3 *pa3;
extern A5 *pa5;
extern A7 *pa7;

extern A7_3 *pa7_3;
extern AX_3 *pax_3;
extern A5_7 *pa5_7;
extern A7_5 *pa7_5;

extern char *ptr;

struct MemArrays0 {
  A7_3 a7_3;
  A5_7 a5_7;
  char a3[3], a5[5], a0[0];
};
struct MemArraysX { char a3[3], a5[5], ax[]; };
struct MemArrays7 { char a3[3], a5[5], a7[7]; };

struct MemArrays0 ma0_3_5_7[3][5][7];

void elim_strings (int i)
{
  ELIM_TRUE (strlen (i < 0 ? "123" : "321") == 3);
  ELIM_FALSE (strlen (i < 0 ? "123" : "321") > 3);
  ELIM_FALSE (strlen (i < 0 ? "123" : "321") < 3);

  ELIM_TRUE (strlen (i < 0 ? "123" : "4321") >= 3);
  ELIM_FALSE (strlen (i < 0 ? "123" : "4321") > 4);
  ELIM_FALSE (strlen (i < 0 ? "123" : "4321") < 3);

  ELIM_TRUE (strlen (i < 0 ? "1234" : "321") >= 3);
  ELIM_FALSE (strlen (i < 0 ? "1234" : "321") < 3);
  ELIM_FALSE (strlen (i < 0 ? "1234" : "321") > 4);

  ELIM_TRUE (strlen (i < 0 ? "123" : "4321") <= 4);
  ELIM_TRUE (strlen (i < 0 ? "1234" : "321") <= 4);

  ELIM_TRUE (strlen (i < 0 ? "1" : "123456789") <= 9);
  ELIM_TRUE (strlen (i < 0 ? "1" : "123456789") >= 1);
}

/* Verify that strlen calls involving uninitialized global arrays
   of known size are eliminated when they appear in expressions
   that test for results that must be true.  */
void elim_global_arrays (int i)
{
  /* Verify that the expression involving the strlen call as well
     as whatever depends on it is eliminated  from the test output.
     All these expressions must be trivially true.  */
  ELIM_TRUE (strlen (a7_3[0]) < sizeof a7_3[0]);
  ELIM_TRUE (strlen (a7_3[1]) < sizeof a7_3[1]);
  ELIM_TRUE (strlen (a7_3[6]) < sizeof a7_3[6]);
  ELIM_TRUE (strlen (a7_3[i]) < sizeof a7_3[i]);

  ELIM_TRUE (strlen (a5_7[0]) < sizeof a5_7[0]);
  ELIM_TRUE (strlen (a5_7[1]) < sizeof a5_7[1]);
  ELIM_TRUE (strlen (a5_7[4]) < sizeof a5_7[4]);
  ELIM_TRUE (strlen (a5_7[i]) < sizeof a5_7[0]);

  ELIM_TRUE (strlen (ax_3[0]) < sizeof ax_3[0]);
  ELIM_TRUE (strlen (ax_3[1]) < sizeof ax_3[1]);
  ELIM_TRUE (strlen (ax_3[9]) < sizeof ax_3[9]);
  ELIM_TRUE (strlen (ax_3[i]) < sizeof ax_3[i]);

  ELIM_TRUE (strlen (a3) < sizeof a3);
  ELIM_TRUE (strlen (a7) < sizeof a7);

  ELIM_TRUE (strlen (ax) != DIFF_MAX);
  ELIM_TRUE (strlen (ax) != DIFF_MAX - 1);
  ELIM_TRUE (strlen (ax) < DIFF_MAX - 1);
}

void elim_pointer_to_arrays (void)
{
  ELIM_TRUE (strlen (*pa7) < 7);
  ELIM_TRUE (strlen (*pa5) < 5);
  ELIM_TRUE (strlen (*pa3) < 3);

  ELIM_TRUE (strlen ((*pa7_3)[0]) < 3);
  ELIM_TRUE (strlen ((*pa7_3)[1]) < 3);
  ELIM_TRUE (strlen ((*pa7_3)[6]) < 3);

  ELIM_TRUE (strlen ((*pax_3)[0]) < 3);
  ELIM_TRUE (strlen ((*pax_3)[1]) < 3);
  ELIM_TRUE (strlen ((*pax_3)[9]) < 3);

  ELIM_TRUE (strlen ((*pa5_7)[0]) < 7);
  ELIM_TRUE (strlen ((*pa5_7)[1]) < 7);
  ELIM_TRUE (strlen ((*pa5_7)[4]) < 7);
}

void elim_global_arrays_and_strings (int i)
{
  ELIM_TRUE (strlen (i < 0 ? a3 : "") < 3);
  ELIM_TRUE (strlen (i < 0 ? a3 : "1") < 3);
  ELIM_TRUE (strlen (i < 0 ? a3 : "12") < 3);
  ELIM_TRUE (strlen (i < 0 ? a3 : "123") < 4);

  ELIM_FALSE (strlen (i < 0 ? a3 : "") > 3);
  ELIM_FALSE (strlen (i < 0 ? a3 : "1") > 3);
  ELIM_FALSE (strlen (i < 0 ? a3 : "12") > 3);
  ELIM_FALSE (strlen (i < 0 ? a3 : "123") > 4);

  ELIM_TRUE (strlen (i < 0 ? a7 : "") < 7);
  ELIM_TRUE (strlen (i < 0 ? a7 : "1") < 7);
  ELIM_TRUE (strlen (i < 0 ? a7 : "12") < 7);
  ELIM_TRUE (strlen (i < 0 ? a7 : "123") < 7);
  ELIM_TRUE (strlen (i < 0 ? a7 : "123456") < 7);
  ELIM_TRUE (strlen (i < 0 ? a7 : "1234567") < 8);

  ELIM_FALSE (strlen (i < 0 ? a7 : "") > 6);
  ELIM_FALSE (strlen (i < 0 ? a7 : "1") > 6);
  ELIM_FALSE (strlen (i < 0 ? a7 : "12") > 6);
  ELIM_FALSE (strlen (i < 0 ? a7 : "123") > 6);
  ELIM_FALSE (strlen (i < 0 ? a7 : "123456") > 7);
  ELIM_FALSE (strlen (i < 0 ? a7 : "1234567") > 8);
}

void elim_member_arrays_obj (int i)
{
  ELIM_TRUE (strlen (ma0_3_5_7[0][0][0].a3) < 3);
  ELIM_TRUE (strlen (ma0_3_5_7[0][0][1].a3) < 3);
  ELIM_TRUE (strlen (ma0_3_5_7[0][0][2].a3) < 3);
  ELIM_TRUE (strlen (ma0_3_5_7[0][0][6].a3) < 3);

  ELIM_TRUE (strlen (ma0_3_5_7[1][0][0].a3) < 3);
  ELIM_TRUE (strlen (ma0_3_5_7[2][0][1].a3) < 3);

  ELIM_TRUE (strlen (ma0_3_5_7[1][1][0].a3) < 3);
  ELIM_TRUE (strlen (ma0_3_5_7[2][4][6].a3) < 3);

  ELIM_TRUE (strlen (ma0_3_5_7[0][0][0].a5) < 5);
  ELIM_TRUE (strlen (ma0_3_5_7[0][0][1].a5) < 5);
  ELIM_TRUE (strlen (ma0_3_5_7[0][0][2].a5) < 5);
  ELIM_TRUE (strlen (ma0_3_5_7[0][0][6].a5) < 5);

  ELIM_TRUE (strlen (ma0_3_5_7[1][0][0].a5) < 5);
  ELIM_TRUE (strlen (ma0_3_5_7[2][0][1].a5) < 5);

  ELIM_TRUE (strlen (ma0_3_5_7[1][1][0].a5) < 5);
  ELIM_TRUE (strlen (ma0_3_5_7[2][4][6].a5) < 5);

  ELIM_TRUE (strlen (ma0_3_5_7[0][0][0].a7_3[0]) < 3);
  ELIM_TRUE (strlen (ma0_3_5_7[2][4][6].a7_3[2]) < 3);

  ELIM_TRUE (strlen (ma0_3_5_7[0][0][0].a5_7[0]) < 7);
  ELIM_TRUE (strlen (ma0_3_5_7[2][4][6].a5_7[4]) < 7);
}

void elim_member_arrays_ptr (struct MemArrays0 *ma0,
			     struct MemArraysX *max,
			     struct MemArrays7 *ma7,
			     int i)
{
  ELIM_TRUE (strlen (ma0->a7_3[0]) < 3);
  ELIM_TRUE (strlen (ma0->a7_3[1]) < 3);
  ELIM_TRUE (strlen (ma0->a7_3[6]) < 3);
  ELIM_TRUE (strlen (ma0->a7_3[6]) < 3);
  ELIM_TRUE (strlen (ma0->a7_3[i]) < 3);
  ELIM_TRUE (strlen (ma0->a7_3[i]) < 3);

  ELIM_TRUE (strlen (ma0->a5_7[0]) < 7);
  ELIM_TRUE (strlen (ma0[0].a5_7[0]) < 7);
  ELIM_TRUE (strlen (ma0[1].a5_7[0]) < 7);
  ELIM_TRUE (strlen (ma0[1].a5_7[4]) < 7);
  ELIM_TRUE (strlen (ma0[9].a5_7[0]) < 7);
  ELIM_TRUE (strlen (ma0[9].a5_7[4]) < 7);

  ELIM_TRUE (strlen (ma0->a3) < sizeof ma0->a3);
  ELIM_TRUE (strlen (ma0->a5) < sizeof ma0->a5);
  ELIM_TRUE (strlen (ma0->a0) < DIFF_MAX - 1);

  ELIM_TRUE (strlen (max->a3) < sizeof max->a3);
  ELIM_TRUE (strlen (max->a5) < sizeof max->a5);
  ELIM_TRUE (strlen (max->ax) < DIFF_MAX - 1);

  ELIM_TRUE (strlen (ma7->a3) < sizeof max->a3);
  ELIM_TRUE (strlen (ma7->a5) < sizeof max->a5);
  ELIM_TRUE (strlen (ma7->a7) < DIFF_MAX - 1);
}


#line 1000

/* Verify that strlen calls involving uninitialized global arrays
   of unknown size are not eliminated when they appear in expressions
   that test for results that need not be true.  */
void keep_global_arrays (int i)
{
  KEEP (strlen (a7_3[0]) < 2);
  KEEP (strlen (a7_3[1]) < 2);
  KEEP (strlen (a7_3[6]) < 2);
  KEEP (strlen (a7_3[i]) < 2);

  KEEP (strlen (a5_7[0]) < 6);
  KEEP (strlen (a5_7[1]) < 6);
  KEEP (strlen (a5_7[4]) < 6);
  KEEP (strlen (a5_7[i]) < 6);

  KEEP (strlen (ax_3[0]) < 2);
  KEEP (strlen (ax_3[1]) < 2);
  KEEP (strlen (ax_3[2]) < 2);
  KEEP (strlen (ax_3[i]) < 2);

  KEEP (strlen (a3) < 2);
  KEEP (strlen (a7) < 6);

  KEEP (strlen (a3 + i) < 2);
  KEEP (strlen (a7 + i) < 2);

  /* The length of an array of unknown size may be as large as
     DIFF_MAX - 2.  */
  KEEP (strlen (ax) != DIFF_MAX - 2);
  KEEP (strlen (ax) < DIFF_MAX - 2);
  KEEP (strlen (ax) < 999);
  KEEP (strlen (ax) < 1);
}

void keep_pointer_to_arrays (void)
{
  KEEP (strlen (*pa7) < 6);
  KEEP (strlen (*pa5) < 4);
  KEEP (strlen (*pa3) < 2);

  KEEP (strlen ((*pa7_3)[0]) < 2);
  KEEP (strlen ((*pa7_3)[1]) < 2);
  KEEP (strlen ((*pa7_3)[6]) < 2);

  KEEP (strlen ((*pax_3)[0]) < 2);
  KEEP (strlen ((*pax_3)[1]) < 2);
  KEEP (strlen ((*pax_3)[9]) < 2);

  KEEP (strlen ((*pa5_7)[0]) < 6);
  KEEP (strlen ((*pa5_7)[1]) < 6);
  KEEP (strlen ((*pa5_7)[4]) < 6);
}

void keep_global_arrays_and_strings (int i)
{
  KEEP (strlen (i < 0 ? a3 : "") < 2);
  KEEP (strlen (i < 0 ? a3 : "1") < 2);
  KEEP (strlen (i < 0 ? a3 : "12") < 2);
  KEEP (strlen (i < 0 ? a3 : "123") < 3);

  KEEP (strlen (i < 0 ? a7 : "") < 5);
  KEEP (strlen (i < 0 ? a7 : "1") < 5);
  KEEP (strlen (i < 0 ? a7 : "12") < 5);
  KEEP (strlen (i < 0 ? a7 : "123") < 5);
  KEEP (strlen (i < 0 ? a7 : "123456") < 6);
  KEEP (strlen (i < 0 ? a7 : "1234567") < 6);
}

void keep_member_arrays_obj (int i)
{
  KEEP (strlen (ma0_3_5_7[0][0][0].a3) < 2);
  KEEP (strlen (ma0_3_5_7[0][0][1].a3) < 2);
  KEEP (strlen (ma0_3_5_7[0][0][2].a3) < 2);
  KEEP (strlen (ma0_3_5_7[0][0][6].a3) < 2);

  KEEP (strlen (ma0_3_5_7[1][0][0].a3) < 2);
  KEEP (strlen (ma0_3_5_7[2][0][1].a3) < 2);

  KEEP (strlen (ma0_3_5_7[1][1][0].a3) < 2);
  KEEP (strlen (ma0_3_5_7[2][4][6].a3) < 2);

  KEEP (strlen (ma0_3_5_7[0][0][0].a5) < 4);
  KEEP (strlen (ma0_3_5_7[0][0][1].a5) < 4);
  KEEP (strlen (ma0_3_5_7[0][0][2].a5) < 4);
  KEEP (strlen (ma0_3_5_7[0][0][6].a5) < 4);

  KEEP (strlen (ma0_3_5_7[1][0][0].a5) < 4);
  KEEP (strlen (ma0_3_5_7[2][0][1].a5) < 4);

  KEEP (strlen (ma0_3_5_7[1][1][0].a5) < 4);
  KEEP (strlen (ma0_3_5_7[2][4][6].a5) < 4);

  KEEP (strlen (ma0_3_5_7[0][0][0].a7_3[0]) < 2);
  KEEP (strlen (ma0_3_5_7[2][4][6].a7_3[2]) < 2);

  KEEP (strlen (ma0_3_5_7[0][0][0].a5_7[0]) < 6);
  KEEP (strlen (ma0_3_5_7[2][4][6].a5_7[4]) < 6);
}

void keep_member_arrays_ptr (struct MemArrays0 *ma0,
			     struct MemArraysX *max,
			     struct MemArrays7 *ma7,
			     int i)
{
  KEEP (strlen (ma0->a7_3[0]) > 0);
  KEEP (strlen (ma0->a7_3[0]) < 2);
  KEEP (strlen (ma0->a7_3[1]) < 2);
  KEEP (strlen (ma0->a7_3[6]) < 2);
  KEEP (strlen (ma0->a7_3[6]) < 2);
  KEEP (strlen (ma0->a7_3[i]) > 0);
  KEEP (strlen (ma0->a7_3[i]) < 2);
  KEEP (strlen (ma0->a7_3[i]) < 2);

  KEEP (strlen (ma0->a5_7[0]) < 5);
  KEEP (strlen (ma0[0].a5_7[0]) < 5);
  KEEP (strlen (ma0[1].a5_7[0]) < 5);
  KEEP (strlen (ma0[9].a5_7[0]) < 5);
  KEEP (strlen (ma0[9].a5_7[4]) < 5);
  KEEP (strlen (ma0[i].a5_7[4]) < 5);
  KEEP (strlen (ma0[i].a5_7[i]) < 5);

  KEEP (strlen (ma0->a0) < DIFF_MAX - 2);
  KEEP (strlen (ma0->a0) < 999);
  KEEP (strlen (ma0->a0) < 1);

  KEEP (strlen (max->ax) < DIFF_MAX - 2);
  KEEP (strlen (max->ax) < 999);
  KEEP (strlen (max->ax) < 1);

  KEEP (strlen (ma7->a7) < DIFF_MAX - 2);
  KEEP (strlen (ma7->a7) < 999);
  KEEP (strlen (ma7->a7) < 1);
}

void keep_pointers (const char *s)
{
  KEEP (strlen (ptr) < DIFF_MAX - 2);
  KEEP (strlen (ptr) < 999);
  KEEP (strlen (ptr) < 1);

  KEEP (strlen (s) < DIFF_MAX - 2);
  KEEP (strlen (s) < 999);
  KEEP (strlen (s) < 1);
}


/* { dg-final { scan-tree-dump-times "call_in_true_branch_not_eliminated_" 0 "optimized" } }
   { dg-final { scan-tree-dump-times "call_in_false_branch_not_eliminated_" 0 "optimized" } }

   { dg-final { scan-tree-dump-times "call_made_in_true_branch_on_line_1\[0-9\]\[0-9\]\[0-9\]" 92 "optimized" } }
   { dg-final { scan-tree-dump-times "call_made_in_false_branch_on_line_1\[0-9\]\[0-9\]\[0-9\]" 92 "optimized" } } */
