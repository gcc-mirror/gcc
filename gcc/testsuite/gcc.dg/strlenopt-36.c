/* PR tree-optimization/78450 - strlen(s) return value can be assumed
   to be less than the size of s
   { dg-do compile }
   { dg-options "-O2 -fdump-tree-optimized" } */

#include "strlenopt.h"

extern char a7[7], a6[6], a5[5], a4[4], a3[3], a2[2], a1[1];
extern char a0[0];   /* Intentionally not tested here.  */
extern char ax[];    /* Same.  */

struct MemArrays {
  char a7[7], a6[6], a5[5], a4[4], a3[3], a2[2], a1[1];
  char a0[0];   /* Not tested here.  */
};

struct NestedMemArrays {
  struct { char a7[7]; } ma7;
  struct { char a6[6]; } ma6;
  struct { char a5[5]; } ma5;
  struct { char a4[4]; } ma4;
  struct { char a3[3]; } ma3;
  struct { char a2[2]; } ma2;
  struct { char a1[1]; } ma1;
  struct { char a0[0]; } ma0;
  char last;
};

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

void test_memarray (struct MemArrays *ma)
{
  T (strlen (ma->a7) < sizeof ma->a7);
  T (strlen (ma->a6) < sizeof ma->a6);
  T (strlen (ma->a5) < sizeof ma->a5);
  T (strlen (ma->a4) < sizeof ma->a4);
  T (strlen (ma->a3) < sizeof ma->a3);

  /* The following two calls are folded too early which defeats
     the strlen() optimization.
  T (strlen (ma->a2) == 1);
  T (strlen (ma->a1) == 0);  */
}

/* Verify that the range of strlen(A) of a last struct member is
   set even when the array is the sole member of a struct as long
   as the struct itself is a member of another struct.  The converse
   is tested in stlenopt-37.c.  */
void test_nested_memarray (struct NestedMemArrays *ma)
{
  T (strlen (ma->ma7.a7) < sizeof ma->ma7.a7);
  T (strlen (ma->ma6.a6) < sizeof ma->ma6.a6);
  T (strlen (ma->ma5.a5) < sizeof ma->ma5.a5);
  T (strlen (ma->ma4.a4) < sizeof ma->ma4.a4);
  T (strlen (ma->ma3.a3) < sizeof ma->ma3.a3);

  /* The following two calls are folded too early which defeats
     the strlen() optimization.
  T (strlen (ma->ma2.a2) == 1);
  T (strlen (ma->ma1.a1) == 0);  */
}

/* { dg-final { scan-tree-dump-not "failure_on_line" "optimized" } } */
